-module(taskflow_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    %% Load configuration
    Port = application:get_env(taskflow, port, 8080),
    
    %% Database connection pool setup
    DbConfig = application:get_env(taskflow, database, []),
    taskflow_db:start_pool(DbConfig),
    
    %% Cowboy routing
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/health", taskflow_health_handler, []},
            {"/api/auth/user", taskflow_auth_handler, [user]},
            {"/api/organizations", taskflow_org_handler, []},
            {"/api/organizations/:id", taskflow_org_handler, [id]},
            {"/api/projects", taskflow_project_handler, []},
            {"/api/projects/:id", taskflow_project_handler, [id]},
            {"/api/tasks", taskflow_task_handler, []},
            {"/api/tasks/:id", taskflow_task_handler, [id]},
            {"/api/billing/subscription", taskflow_billing_handler, [subscription]},
            {"/api/billing/create-checkout", taskflow_billing_handler, [checkout]},
            {"/api/billing/webhook", taskflow_stripe_webhook_handler, []},
            {"/api/[...]", taskflow_cors_handler, []}
        ]}
    ]),
    
    %% Start Cowboy
    {ok, _} = cowboy:start_clear(taskflow_http_listener,
        [{port, Port}],
        #{env => #{dispatch => Dispatch}}
    ),
    
    taskflow_sup:start_link().

stop(_State) ->
    ok.