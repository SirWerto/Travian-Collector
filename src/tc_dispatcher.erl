%%%-------------------------------------------------------------------
%% @doc tc dispatcher.
%% @end
%%%-------------------------------------------------------------------

-module(tc_dispatcher).

-behaviour(gen_statem).

-export([start_link/1, get_servers_list/0]).

-export([init/1, callback_mode/0]).

-export([scraping/3, collecting/3, waiting/3]).

-define(SPEC, #{id => wsup,
	        start => {tc_sup_workers, start_link, []},
	        restart => temporary,
	        shutdown => 10000,
	        type => supervisor,
	        modules => [tc_sup_workers]}).

-record(s, {supervisor,
	    tasks=queue:new(),
	    ntask = 0,
	    monitors=maps:new(),
	    nmoni = 0,
	    done = [],
	    failed = [],
	    maxTries=6,
	    maxWorkers=40,
	    daily={9,0,0}}).


start_link(Parent) ->
    gen_statem:start_link(?MODULE, [Parent], []).

init([Parent]) ->
    self() ! {start_sup, Parent},
    case application:get_env(start_on_launch) of
	{ok, true} -> {ok, scraping, []};
	{ok, false} -> {ok, waiting, []}
    end.

callback_mode() ->
    state_functions.


%% SCRAPING STATE
scraping(info, {start_sup, Parent}, _S) ->
    {ok, Sup} = supervisor:start_child(Parent, ?SPEC),
    {ok, Tries} = application:get_env(maxTries),
    {ok, Workers} = application:get_env(maxWorkers),
    {ok, Daily} = application:get_env(daily_launch),
    Hour = maps:get(hour, Daily),
    Minute = maps:get(minute, Daily),
    Second = maps:get(second, Daily),
    S = #s{supervisor=Sup,
	   maxTries = Tries,
	   maxWorkers = Workers,
	   daily = {Hour, Minute, Second}},
    {next_state, scraping, S, [{state_timeout, 10000, scrap_now}]};

scraping(state_timeout, scrap_now, S) ->
    Fn = fun(E, Acc) -> queue:in({E, 0}, Acc) end,
    NewTasks = lists:foldl(Fn, queue:new(), get_servers_list()),
    NewNtask = queue:len(NewTasks),
    NewS = S#s{tasks=NewTasks, ntask=NewNtask},
    io:format("Scrap now~p~n",[NewTasks]),
    {next_state, collecting, NewS, [{state_timeout, 100, launch_collecting}]}.



%% COLLECTING STATE
collecting(state_timeout, launch_collecting, S) ->
    NewState = send_task(S),
    {next_state, collecting, NewState};
collecting(info, {'DOWN', Ref, process, _Pid, normal}, S) ->
    case send_task(handle_done(Ref, S)) of
	{job_done, NewState} ->
	    {next_state, waiting, NewState, [{state_timeout, 100, wait_until_daily}]};
	NewState ->
	    {next_state, collecting, NewState}
    end;
collecting(info, {'DOWN', Ref, process, _Pid, _Reason}, S) ->
    case send_task(handle_down(Ref, S)) of
	{job_done, NewState} ->
	    {next_state, waiting, NewState, [{state_timeout, 100, wait_until_daily}]};
	NewState ->
	    {next_state, collecting, NewState}
    end.

    


%% WAITING STATE
waiting(info, {start_sup, Parent}, _S) ->
    {ok, Sup} = supervisor:start_child(Parent, ?SPEC),
    {ok, Tries} = application:get_env(maxTries),
    {ok, Workers} = application:get_env(maxWorkers),
    {ok, Daily} = application:get_env(daily_launch),
    Hour = maps:get(hour, Daily),
    Minute = maps:get(minute, Daily),
    Second = maps:get(second, Daily),
    S = #s{supervisor=Sup,
	   maxTries = Tries,
	   maxWorkers = Workers,
	   daily = {Hour, Minute, Second}},
    {next_state, waiting, S, [{state_timeout, 10000, wait_until_daily}]};

waiting(state_timeout, wait_until_daily, S = #s{daily=D}) ->
    STime = time_until_daily(D),
    timer:sleep(STime),
    {next_state, scraping, S, [{state_timeout, 100, scrap_now}]}.


    
%% Internal Functions

time_until_daily({H, M, S}) ->
    {_Day, {NH, NM, NS}} = calendar:local_time(),
    {DH, DM, DS} = eval_diff(H-NH, M-NM, S-NS),
    (DH*60*60 + DM*60 + DS)*1000.

eval_diff(H, M, S) ->
    if S < 0 ->
	    NewS = 60 + S,
	    NewM = M -1,
	    eval_diff(H, NewM, NewS);
       M < 0 ->
	    NewM = 60 + M,
	    NewH = H -1,
	    eval_diff(NewH, NewM, S);
       H < 0 ->
	    NewH = 24 + H,
	    {NewH, M, S};
       true ->
	    {H, M, S}
    end.

%% State stuff

send_task(State = #s{ntask=0, nmoni=0}) ->
    {job_done, State};
send_task(State = #s{ntask=0}) ->
    State;
send_task(State = #s{nmoni=Max, maxWorkers=Max}) ->
    State;
send_task(State = #s{tasks=Tasks, ntask=NTask, supervisor=Sup, monitors=Monitors, nmoni=NM}) ->
    {{value, {Task, Count}}, NewTasks} = queue:out(Tasks),
    {ok, Pid} = supervisor:start_child(Sup, [Task]),
    Ref = erlang:monitor(process, Pid),
    NewMonitors = maps:put(Ref, {Task,Count+1}, Monitors),
    NewState = State#s{tasks=NewTasks, ntask=NTask-1, monitors=NewMonitors, nmoni=NM+1},
    NewState.

handle_done(Ref, State = #s{monitors=Monitors, nmoni=NM, done=Done}) ->
    case maps:is_key(Ref, Monitors) of
	true ->
	    Task = maps:get(Ref, Monitors),
	    NewDone = [Task | Done],
	    NewMonitors = maps:remove(Ref, Monitors),
	    State#s{monitors=NewMonitors, nmoni=NM-1, done=NewDone};
	false ->
	    State
    end.
	    
handle_down(Ref, State = #s{tasks=Tasks, ntask=NTask, monitors=Monitors, nmoni=NM, maxTries=MaxTries, failed=Failed}) ->
    case maps:is_key(Ref, Monitors) of
	true ->
	    {Task, Count} = maps:get(Ref, Monitors),
	    NewMonitors = maps:remove(Ref, Monitors),
	    if Count =:= MaxTries ->
		    NewFailed = [Task | Failed],
		    State#s{monitors=NewMonitors, nmoni=NM-1, failed=NewFailed};
	       true ->
		    NewTasks = queue:in({Task, Count+1}, Tasks),
		    State#s{tasks=NewTasks, ntask=NTask-1, monitors=NewMonitors, nmoni=NM-1}
	    end;
	false ->
	    State
    end.
		    
	    
    
%%% Request Servers Functions
    
get_servers_html() ->
    Url = "https://status.travian.com/",
    {ok, {{_, 200, _}, _, Body}} = httpc:request(get, {Url, []}, [], []),
    Body.

parse_server_html(S) ->
    [_| Splited] = string:split(S, "https://", all),
    [get_server(X) || X <- Splited].

get_server(S) ->
    [Splited| _] = string:split(S, "<", leading),
    Splited.

get_servers_list() ->
    parse_server_html(get_servers_html()).
