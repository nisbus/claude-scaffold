%%%-------------------------------------------------------------------
%%% @doc
%%% Database connection pool for Erlang applications
%%% Handles both Neon and standard PostgreSQL connections
%%% Based on chimera_db patterns with epgsql and poolboy
%%% @end
%%%-------------------------------------------------------------------
-module(db_connection_pool).

-export([start_link/0, start_link/1, query/2, equery/2, transaction/1]).
-export([checkout/0, checkin/1, execute/2]).

-define(POOL_NAME, db_pool).
-define(DEFAULT_POOL_SIZE, 10).
-define(DEFAULT_MAX_OVERFLOW, 10).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the database pool with default configuration
start_link() ->
    start_link([]).

%% @doc Start the database pool with custom configuration
start_link(Config) ->
    % Get configuration from environment or defaults
    DbConfig = get_db_config(Config),
    
    % Poolboy configuration
    PoolArgs = [
        {name, {local, ?POOL_NAME}},
        {worker_module, db_worker},
        {size, proplists:get_value(pool_size, Config, ?DEFAULT_POOL_SIZE)},
        {max_overflow, proplists:get_value(max_overflow, Config, ?DEFAULT_MAX_OVERFLOW)}
    ],
    
    % Start poolboy with our worker
    poolboy:start_link(PoolArgs, DbConfig).

%% @doc Execute a simple query
-spec query(string(), list()) -> {ok, list()} | {error, term()}.
query(Query, Params) ->
    poolboy:transaction(?POOL_NAME, fun(Worker) ->
        gen_server:call(Worker, {query, Query, Params})
    end).

%% @doc Execute an extended query (prepared statement)
-spec equery(string(), list()) -> {ok, list()} | {error, term()}.
equery(Query, Params) ->
    poolboy:transaction(?POOL_NAME, fun(Worker) ->
        gen_server:call(Worker, {equery, Query, Params})
    end).

%% @doc Execute a function within a database transaction
-spec transaction(fun()) -> {ok, term()} | {error, term()}.
transaction(Fun) ->
    poolboy:transaction(?POOL_NAME, fun(Worker) ->
        gen_server:call(Worker, {transaction, Fun})
    end).

%% @doc Execute SQL directly (bypasses pool if needed)
-spec execute(string(), list()) -> {ok, list()} | {error, term()}.
execute(SQL, Params) ->
    try
        equery(SQL, Params)
    catch
        _:_ ->
            % Fallback to direct connection if pool not available
            execute_direct(SQL, Params)
    end.

%% @doc Checkout a worker from the pool
-spec checkout() -> {ok, pid()} | {error, term()}.
checkout() ->
    try
        Worker = poolboy:checkout(?POOL_NAME),
        {ok, Worker}
    catch
        exit:{timeout, _} ->
            {error, pool_timeout};
        exit:{noproc, _} ->
            {error, pool_not_available};
        _:Reason ->
            {error, {checkout_failed, Reason}}
    end.

%% @doc Checkin a worker to the pool
-spec checkin(pid()) -> ok.
checkin(Worker) ->
    try
        poolboy:checkin(?POOL_NAME, Worker)
    catch
        _:_ -> ok
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
%% Get database configuration from various sources
get_db_config(Config) ->
    % Priority: 1. Passed config, 2. DATABASE_URL, 3. Individual env vars, 4. Defaults
    case os:getenv("DATABASE_URL") of
        false ->
            get_config_from_env_vars(Config);
        DatabaseUrl ->
            parse_database_url(DatabaseUrl)
    end.

%% @private
%% Get config from individual environment variables
get_config_from_env_vars(Config) ->
    Host = get_env_or_config("DB_HOST", host, Config, "localhost"),
    Port = get_env_or_config("DB_PORT", port, Config, 5432),
    Database = get_env_or_config("DB_NAME", database, Config, "app_db"),
    Username = get_env_or_config("DB_USER", username, Config, "postgres"),
    Password = get_env_or_config("DB_PASS", password, Config, "postgres"),
    
    BaseConfig = [
        {hostname, Host},
        {port, ensure_integer(Port)},
        {database, Database},
        {username, Username},
        {password, Password}
    ],
    
    % Add SSL configuration for non-localhost connections
    apply_ssl_config(Host, BaseConfig).

%% @private
%% Parse DATABASE_URL format: postgresql://user:password@host:port/database?params
parse_database_url(DatabaseUrl) ->
    case string:split(DatabaseUrl, "://", trailing) of
        [_Proto, Rest] ->
            % Remove query parameters and parse URL components
            UrlWithoutParams = remove_query_params(Rest),
            {User, Pass, Host, Port, Database} = parse_url_components(UrlWithoutParams),
            
            BaseConfig = [
                {hostname, Host},
                {port, Port},
                {database, Database},
                {username, User},
                {password, Pass}
            ],
            
            % Apply SSL and Neon-specific configurations
            apply_connection_config(Host, Pass, BaseConfig);
        _ ->
            error({invalid_database_url, DatabaseUrl})
    end.

%% @private
%% Remove query parameters from URL
remove_query_params(Url) ->
    case string:split(Url, "?", leading) of
        [MainPart | _] -> MainPart;
        _ -> Url
    end.

%% @private
%% Parse URL components: user:pass@host:port/database
parse_url_components(Url) ->
    case string:split(Url, "/", trailing) of
        [AuthAndHost, Database] ->
            case string:split(AuthAndHost, "@", trailing) of
                [Auth, HostPort] ->
                    % Parse auth
                    [User, Password] = case string:split(Auth, ":", leading) of
                        [U, P] -> [U, P];
                        [U] -> [U, ""]
                    end,
                    
                    % Parse host and port
                    {Host, Port} = case string:split(HostPort, ":", trailing) of
                        [H, P] -> {H, list_to_integer(P)};
                        [H] -> {H, 5432}
                    end,
                    
                    {User, Password, Host, Port, Database};
                _ ->
                    error(invalid_database_url_format)
            end;
        _ ->
            error(invalid_database_url_format)
    end.

%% @private
%% Apply SSL configuration based on host
apply_ssl_config("localhost", Config) ->
    Config;
apply_ssl_config(_Host, Config) ->
    % Non-localhost means cloud database - enable SSL
    Config ++ [
        {ssl, true},
        {ssl_opts, [{verify, verify_none}]}
    ].

%% @private
%% Apply connection configuration for Neon and other cloud databases
apply_connection_config(Host, Password, BaseConfig) ->
    ConfigWithSsl = apply_ssl_config(Host, BaseConfig),
    
    % Check if this is a Neon database
    case string:find(Host, "neon.tech") of
        nomatch ->
            ConfigWithSsl;
        _ ->
            % Neon database - handle endpoint parameter
            apply_neon_config(Host, Password, ConfigWithSsl)
    end.

%% @private
%% Apply Neon-specific configuration
apply_neon_config(Host, Password, Config) ->
    % Extract endpoint ID (first part of hostname)
    EndpointId = hd(string:split(Host, ".")),
    
    % Neon requires endpoint parameter in password field
    ModifiedPassword = "endpoint=" ++ EndpointId ++ ";" ++ Password,
    
    % Update password in config
    lists:keyreplace(password, 1, Config, {password, ModifiedPassword}).

%% @private
%% Helper to get environment variable or config value
get_env_or_config(EnvVar, ConfigKey, Config, Default) ->
    case os:getenv(EnvVar) of
        false ->
            proplists:get_value(ConfigKey, Config, Default);
        Value ->
            Value
    end.

%% @private
%% Ensure value is integer
ensure_integer(Value) when is_integer(Value) -> Value;
ensure_integer(Value) when is_list(Value) -> list_to_integer(Value);
ensure_integer(Value) when is_binary(Value) -> binary_to_integer(Value).

%% @private
%% Execute query directly without pool
execute_direct(SQL, Params) ->
    Config = get_db_config([]),
    case epgsql:connect(Config) of
        {ok, Conn} ->
            try
                Result = epgsql:equery(Conn, SQL, Params),
                format_result(Result)
            after
                epgsql:close(Conn)
            end;
        {error, Reason} ->
            {error, {connection_failed, Reason}}
    end.

%% @private
%% Format epgsql result to standard format
format_result({ok, Columns, Rows}) ->
    ColumnNames = [element(2, Col) || Col <- Columns],
    Maps = [maps:from_list(lists:zip(ColumnNames, tuple_to_list(Row))) || Row <- Rows],
    {ok, Maps};
format_result({ok, _Count}) ->
    {ok, []};
format_result({ok, _Count, Columns, Rows}) ->
    ColumnNames = [element(2, Col) || Col <- Columns],
    Maps = [maps:from_list(lists:zip(ColumnNames, tuple_to_list(Row))) || Row <- Rows],
    {ok, Maps};
format_result({error, Reason}) ->
    {error, Reason}.