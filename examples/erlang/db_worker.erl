%%%-------------------------------------------------------------------
%%% @doc
%%% Database worker for poolboy - handles actual database connections
%%% Supports Neon, Render PostgreSQL, and standard PostgreSQL
%%% @end
%%%-------------------------------------------------------------------
-module(db_worker).

-behaviour(gen_server).
-behaviour(poolboy_worker).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

-record(state, {
    conn :: pid() | undefined,
    config :: list()
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start a worker process for poolboy
-spec start_link(list()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link(?MODULE, Config, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init(Config) ->
    process_flag(trap_exit, true),
    
    % Connect to database
    case connect_with_retry(Config, 3) of
        {ok, Conn} ->
            % Set schema if needed (useful for multi-tenant apps)
            maybe_set_schema(Conn, Config),
            {ok, #state{conn = Conn, config = Config}};
        {error, Reason} ->
            error_logger:error_msg("Failed to connect to database: ~p~n", [Reason]),
            {stop, {connection_failed, Reason}}
    end.

%% @private
%% Handle query execution
handle_call({query, Query, Params}, _From, #state{conn = Conn} = State) ->
    Result = execute_query(Conn, Query, Params, State),
    {reply, Result, State};

%% Handle extended query (prepared statement)
handle_call({equery, Query, Params}, _From, #state{conn = Conn} = State) ->
    Result = execute_equery(Conn, Query, Params, State),
    {reply, Result, State};

%% Handle transaction
handle_call({transaction, Fun}, _From, #state{conn = Conn} = State) ->
    Result = execute_transaction(Conn, Fun, State),
    {reply, Result, State};

%% Handle connection check
handle_call(ping, _From, #state{conn = Conn} = State) ->
    case epgsql:squery(Conn, "SELECT 1") of
        {ok, _, _} ->
            {reply, ok, State};
        _ ->
            % Try to reconnect
            case reconnect(State) of
                {ok, NewState} ->
                    {reply, ok, NewState};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
%% Handle connection loss
handle_info({'EXIT', Conn, _Reason}, #state{conn = Conn} = State) ->
    error_logger:warning_msg("Database connection lost, attempting reconnect...~n"),
    case reconnect(State) of
        {ok, NewState} ->
            {noreply, NewState};
        {error, Reason} ->
            error_logger:error_msg("Failed to reconnect: ~p~n", [Reason]),
            {stop, {connection_lost, Reason}, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, #state{conn = Conn}) when is_pid(Conn) ->
    epgsql:close(Conn),
    ok;
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
%% Connect with retry logic
connect_with_retry(_Config, 0) ->
    {error, max_retries_exceeded};
connect_with_retry(Config, Retries) ->
    case epgsql:connect(Config) of
        {ok, Conn} ->
            {ok, Conn};
        {error, Reason} when Retries > 1 ->
            error_logger:warning_msg("Connection attempt failed (~p retries left): ~p~n", 
                                    [Retries - 1, Reason]),
            timer:sleep(1000), % Wait 1 second before retry
            connect_with_retry(Config, Retries - 1);
        {error, Reason} ->
            {error, Reason}
    end.

%% @private
%% Set schema if specified in config
maybe_set_schema(Conn, Config) ->
    case proplists:get_value(schema, Config) of
        undefined ->
            ok;
        Schema ->
            Query = io_lib:format("SET search_path TO ~s", [Schema]),
            case epgsql:squery(Conn, Query) of
                {ok, _, _} ->
                    ok;
                Error ->
                    error_logger:warning_msg("Failed to set schema ~s: ~p~n", [Schema, Error]),
                    ok
            end
    end.

%% @private
%% Execute simple query
execute_query(Conn, Query, Params, State) ->
    try
        Result = epgsql:equery(Conn, Query, Params),
        format_result(Result)
    catch
        error:badarg ->
            % Connection might be dead, try to reconnect
            case reconnect(State) of
                {ok, #state{conn = NewConn}} ->
                    Result = epgsql:equery(NewConn, Query, Params),
                    format_result(Result);
                {error, Reason} ->
                    {error, {connection_error, Reason}}
            end;
        Type:Error ->
            {error, {Type, Error}}
    end.

%% @private
%% Execute extended query (prepared statement)
execute_equery(Conn, Query, Params, State) ->
    try
        % For better performance, we could cache prepared statements
        Result = epgsql:equery(Conn, Query, Params),
        format_result(Result)
    catch
        error:badarg ->
            case reconnect(State) of
                {ok, #state{conn = NewConn}} ->
                    Result = epgsql:equery(NewConn, Query, Params),
                    format_result(Result);
                {error, Reason} ->
                    {error, {connection_error, Reason}}
            end;
        Type:Error ->
            {error, {Type, Error}}
    end.

%% @private
%% Execute transaction
execute_transaction(Conn, Fun, State) ->
    try
        case epgsql:squery(Conn, "BEGIN") of
            {ok, _, _} ->
                try
                    Result = Fun(Conn),
                    case epgsql:squery(Conn, "COMMIT") of
                        {ok, _, _} ->
                            {ok, Result};
                        Error ->
                            epgsql:squery(Conn, "ROLLBACK"),
                            {error, {commit_failed, Error}}
                    end
                catch
                    Type:Error ->
                        epgsql:squery(Conn, "ROLLBACK"),
                        {error, {transaction_error, Type, Error}}
                end;
            Error ->
                {error, {begin_failed, Error}}
        end
    catch
        error:badarg ->
            case reconnect(State) of
                {ok, NewState} ->
                    execute_transaction(NewState#state.conn, Fun, NewState);
                {error, Reason} ->
                    {error, {connection_error, Reason}}
            end
    end.

%% @private
%% Reconnect to database
reconnect(#state{config = Config}) ->
    case connect_with_retry(Config, 3) of
        {ok, Conn} ->
            maybe_set_schema(Conn, Config),
            {ok, #state{conn = Conn, config = Config}};
        {error, Reason} ->
            {error, Reason}
    end.

%% @private
%% Format epgsql result to standard format
format_result({ok, Columns, Rows}) ->
    ColumnNames = extract_column_names(Columns),
    Maps = rows_to_maps(ColumnNames, Rows),
    {ok, Maps};
format_result({ok, Count}) when is_integer(Count) ->
    {ok, [{affected_rows, Count}]};
format_result({ok, Count, Columns, Rows}) ->
    ColumnNames = extract_column_names(Columns),
    Maps = rows_to_maps(ColumnNames, Rows),
    {ok, [{affected_rows, Count} | Maps]};
format_result({error, {error, error, _, Msg, _}}) ->
    {error, binary_to_list(Msg)};
format_result({error, Reason}) ->
    {error, Reason}.

%% @private
%% Extract column names from epgsql column format
extract_column_names(Columns) ->
    [case Col of
        {column, Name, _, _, _, _, _} -> Name;
        {column, Name, _, _, _, _} -> Name;
        _ -> element(2, Col)
     end || Col <- Columns].

%% @private
%% Convert rows to maps
rows_to_maps(_, []) ->
    [];
rows_to_maps(ColumnNames, Rows) ->
    [maps:from_list(lists:zip(ColumnNames, tuple_to_list(Row))) || Row <- Rows].