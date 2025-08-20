-module(taskflow_task_handler).
-behaviour(cowboy_handler).

-export([init/2, allowed_methods/2, content_types_provided/2, content_types_accepted/2]).
-export([handle_get/2, handle_post/2, handle_put/2, handle_delete/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>, <<"OPTIONS">>], Req, State}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, handle_get}], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, handle_post}], Req, State}.

%% GET /api/tasks - List tasks
handle_get(Req0, State) ->
    %% Verify JWT token
    case taskflow_auth:verify_token(Req0) of
        {ok, UserId} ->
            ProjectId = cowboy_req:qs_val(<<"project_id">>, Req0),
            case ProjectId of
                undefined ->
                    %% Get all tasks for user's projects
                    Tasks = taskflow_db:get_user_tasks(UserId),
                    taskflow_response:json(Req0, State, #{tasks => Tasks});
                ProjId ->
                    %% Get tasks for specific project
                    case taskflow_auth:can_access_project(UserId, ProjId) of
                        true ->
                            Tasks = taskflow_db:get_project_tasks(ProjId),
                            taskflow_response:json(Req0, State, #{tasks => Tasks});
                        false ->
                            taskflow_response:error(Req0, State, 403, <<"Access denied">>)
                    end
            end;
        {error, _} ->
            taskflow_response:error(Req0, State, 401, <<"Unauthorized">>)
    end.

%% POST /api/tasks - Create task
handle_post(Req0, State) ->
    case taskflow_auth:verify_token(Req0) of
        {ok, UserId} ->
            {ok, Body, Req1} = cowboy_req:read_body(Req0),
            case jsx:decode(Body, [return_maps]) of
                #{<<"title">> := Title, <<"project_id">> := ProjectId} = TaskData ->
                    %% Verify user can create tasks in this project
                    case taskflow_auth:can_access_project(UserId, ProjectId) of
                        true ->
                            TaskParams = #{
                                title => Title,
                                description => maps:get(<<"description">>, TaskData, <<"">>),
                                project_id => ProjectId,
                                assigned_to => maps:get(<<"assigned_to">>, TaskData, UserId),
                                priority => maps:get(<<"priority">>, TaskData, <<"medium">>),
                                due_date => maps:get(<<"due_date">>, TaskData, null),
                                created_by => UserId
                            },
                            case taskflow_db:create_task(TaskParams) of
                                {ok, Task} ->
                                    %% Send real-time notification
                                    taskflow_notifications:task_created(Task),
                                    taskflow_response:json(Req1, State, #{task => Task});
                                {error, Reason} ->
                                    taskflow_response:error(Req1, State, 400, Reason)
                            end;
                        false ->
                            taskflow_response:error(Req1, State, 403, <<"Access denied">>)
                    end;
                _ ->
                    taskflow_response:error(Req1, State, 400, <<"Invalid request body">>)
            end;
        {error, _} ->
            taskflow_response:error(Req0, State, 401, <<"Unauthorized">>)
    end.

%% PUT /api/tasks/:id - Update task
handle_put(Req0, State) ->
    TaskId = cowboy_req:binding(id, Req0),
    case taskflow_auth:verify_token(Req0) of
        {ok, UserId} ->
            case taskflow_auth:can_edit_task(UserId, TaskId) of
                true ->
                    {ok, Body, Req1} = cowboy_req:read_body(Req0),
                    Updates = jsx:decode(Body, [return_maps]),
                    case taskflow_db:update_task(TaskId, Updates) of
                        {ok, Task} ->
                            %% Send real-time update
                            taskflow_notifications:task_updated(Task),
                            taskflow_response:json(Req1, State, #{task => Task});
                        {error, not_found} ->
                            taskflow_response:error(Req1, State, 404, <<"Task not found">>);
                        {error, Reason} ->
                            taskflow_response:error(Req1, State, 400, Reason)
                    end;
                false ->
                    taskflow_response:error(Req0, State, 403, <<"Access denied">>)
            end;
        {error, _} ->
            taskflow_response:error(Req0, State, 401, <<"Unauthorized">>)
    end.

%% DELETE /api/tasks/:id - Delete task
handle_delete(Req0, State) ->
    TaskId = cowboy_req:binding(id, Req0),
    case taskflow_auth:verify_token(Req0) of
        {ok, UserId} ->
            case taskflow_auth:can_delete_task(UserId, TaskId) of
                true ->
                    case taskflow_db:delete_task(TaskId) of
                        ok ->
                            taskflow_notifications:task_deleted(TaskId),
                            taskflow_response:json(Req0, State, #{status => <<"deleted">>});
                        {error, not_found} ->
                            taskflow_response:error(Req0, State, 404, <<"Task not found">>);
                        {error, Reason} ->
                            taskflow_response:error(Req0, State, 400, Reason)
                    end;
                false ->
                    taskflow_response:error(Req0, State, 403, <<"Access denied">>)
            end;
        {error, _} ->
            taskflow_response:error(Req0, State, 401, <<"Unauthorized">>)
    end.