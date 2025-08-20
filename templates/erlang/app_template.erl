%%%-------------------------------------------------------------------
%%% @doc {{app_name}} public API
%%% @end
%%%-------------------------------------------------------------------
-module({{app_name}}_app).

-behaviour(application).

-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    Port = application:get_env({{app_name}}, port, 8080),
    
    %% Database connection
    {ok, _} = {{app_name}}_db:start_link(),
    
    %% Auth0 configuration
    {{app_name}}_auth:init(),
    
    %% Stripe configuration
    {{app_name}}_stripe:init(),
    
    %% Cowboy routes
    Dispatch = cowboy_router:compile([
        {'_', [
            %% Health check
            {"/health", {{app_name}}_health_handler, []},
            
            %% API routes
            {"/api/auth/[...]", {{app_name}}_auth_handler, []},
            {"/api/users/[...]", {{app_name}}_user_handler, []},
            {"/api/billing/[...]", {{app_name}}_billing_handler, []},
            {"/api/webhooks/stripe", {{app_name}}_stripe_webhook_handler, []},
            
            %% WebSocket endpoint
            {"/ws", {{app_name}}_websocket_handler, []},
            
            %% Static files (if needed)
            {"/[...]", cowboy_static, {priv_dir, {{app_name}}, "static"}}
        ]}
    ]),
    
    {ok, _} = cowboy:start_clear(http_listener,
        [{port, Port}],
        #{env => #{dispatch => Dispatch}}
    ),
    
    io:format("~n🚀 {{app_name}} started on port ~p~n", [Port]),
    {{app_name}}_sup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(http_listener),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================