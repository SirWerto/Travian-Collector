%%%-------------------------------------------------------------------
%% @doc tc dispatcher.
%% @end
%%%-------------------------------------------------------------------

-module(tc_dispatcher).

-behaviour(gen_statem).

-ifdef(EXPORTALL).
-compile(export_all).
-endif.

-export([start_link/1]).

-export([init/1, callback_mode/0]).

-export([scraping/3, collecting/3, waiting/3]).


-define(SPEC, #{id => wsup,
	        start => {tc_sup_workers, start_link, []},
	        restart => temporary,
	        shutdown => 10000,
	        type => supervisor,
	        modules => [tc_sup_workers]}).

-type task() :: {Url :: binary(), Count :: non_neg_integer()}.

-record(s, {supervisor :: pid(),
	    tasks=queue:new() :: queue:queue(),
	    ntask = 0 :: non_neg_integer(),
	    monitors=maps:new() :: map(),
	    nmoni = 0 :: non_neg_integer(),
	    tdir :: string(),
	    done = [] :: list(task()),
	    failed = [] :: list(task()),
	    maxTries=6 :: pos_integer(),
	    maxWorkers=40 :: pos_integer(),
	    daily={9,0,0} :: {0..24, 0..59, 0..59}}).

-type s() :: #s{}.


-spec start_link(Parent :: pid()) -> {ok, pid()} | {error, any()}.
start_link(Parent) ->
    gen_statem:start_link({local, travian_dp}, ?MODULE, [Parent], []).

-spec init([Parent :: pid()]) -> {ok, scraping, []} | {ok, waiting, []}.
init([Parent]) ->
    self() ! {start_sup, Parent},
    case application:get_env(start_on_launch) of
	{ok, true} -> {ok, scraping, []};
	{ok, false} -> {ok, waiting, []}
    end.

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.


%% SCRAPING STATE
scraping(info, {start_sup, Parent}, _S) ->
    {ok, Sup} = supervisor:start_child(Parent, ?SPEC),
    {ok, Tries} = application:get_env(maxTries),
    {ok, Workers} = application:get_env(maxWorkers),
    {ok, TDir} = application:get_env(tdir),
    ok = filelib:ensure_dir(TDir),
    {ok, Daily} = application:get_env(daily_launch),
    Hour = maps:get(hour, Daily),
    Minute = maps:get(minute, Daily),
    Second = maps:get(second, Daily),
    S = #s{supervisor=Sup,
	   tdir = TDir,
	   maxTries = Tries,
	   maxWorkers = Workers,
	   daily = {Hour, Minute, Second}},
    {next_state, scraping, S, [{state_timeout, 1000, scrap_now}]};

scraping(state_timeout, scrap_now, S) ->
    case get_servers_list() of
	{ok, Servers} ->
	    Fn = fun(E, Acc) -> queue:in({E, 0}, Acc) end,
	    NewTasks = lists:foldl(Fn, queue:new(), Servers),
	    NewNtask = queue:len(NewTasks),
	    NewS = S#s{tasks=NewTasks, ntask=NewNtask},
	    io:format("Scrap now~n",[]),
	    {next_state, collecting, NewS, [{state_timeout, 100, launch_collecting}]};
	{error, Reason} ->
	    io:format("Can't get servers status ~p~n", [Reason]),
	    {next_state, scraping, S, [{state_timeout, 5000, scrap_now}]}
    end.


%% COLLECTING STATE
collecting(state_timeout, launch_collecting, S) ->
    NewState = send_task(S),
    io:format("Task sended~n",[]),
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
    {ok, TDir} = application:get_env(tdir),
    ok = filelib:ensure_dir(TDir),
    {ok, Daily} = application:get_env(daily_launch),
    Hour = maps:get(hour, Daily),
    Minute = maps:get(minute, Daily),
    Second = maps:get(second, Daily),
    S = #s{supervisor=Sup,
	   maxTries = Tries,
	   tdir = TDir,
	   maxWorkers = Workers,
	   daily = {Hour, Minute, Second}},
    {next_state, waiting, S, [{state_timeout, 1000, wait_until_daily}]};

waiting(state_timeout, wait_until_daily, S = #s{daily=D}) ->
    io:format("Collecting finished",[]),
    DT = calendar:local_time(),
    STime = time_difference(D, DT),
    timer:sleep(STime*1000),
    {next_state, scraping, S, [{state_timeout, 100, scrap_now}]}.


    
%% Internal Functions

-spec time_difference(ObjTime :: calendar:time(), CDT :: calendar:datetime()) -> non_neg_integer().
time_difference(ObjTime, CDT = {CurrentDate, _CurrentTime}) ->
    DatePlusOne = add_one_day(CurrentDate),
    calendar:datetime_to_gregorian_seconds({DatePlusOne, ObjTime}) 
	- calendar:datetime_to_gregorian_seconds(CDT).

-spec add_one_day({Year :: non_neg_integer(), Month :: 1..12, Day :: 1..31}) -> {non_neg_integer(), 1..12, 1..31}.
add_one_day({Year, 12, Day}) ->
    case calendar:last_day_of_the_month(Year, 12) of
	Day ->
	    {Year+1, 1, 1};
	_ -> {Year, 12, Day+1}
    end;
add_one_day({Year, Month, Day}) ->
    case calendar:last_day_of_the_month(Year, Month) of
	Day ->
	    {Year, Month+1, 1};
	_ -> {Year, Month, Day+1}
    end.


    

%% State stuff

-spec send_task(State :: s()) -> {job_done, s()} | s().
send_task(State = #s{ntask=0, nmoni=0}) ->
    {job_done, State};
send_task(State = #s{ntask=0}) ->
    State;
send_task(State = #s{nmoni=Max, maxWorkers=Max}) ->
    State;
send_task(State = #s{tasks=Tasks, ntask=NTask, supervisor=Sup, tdir=TDir, monitors=Monitors, nmoni=NM}) ->
    {{value, {Task, Count}}, NewTasks} = queue:out(Tasks),
    {ok, Pid} = supervisor:start_child(Sup, [Task, TDir]),
    Ref = erlang:monitor(process, Pid),
    NewMonitors = maps:put(Ref, {Task,Count+1}, Monitors),
    NewState = State#s{tasks=NewTasks, ntask=NTask-1, monitors=NewMonitors, nmoni=NM+1},
    NewState.

-spec handle_done(Ref :: reference(), State :: s()) -> s().
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
	    
-spec handle_down(Ref :: reference(), State :: s()) -> s().
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
    
-spec get_servers_html() -> binary() | {error, any()}.
get_servers_html() ->
    case hackney:request(get, <<"https://status.travian.com">>, [], <<>>, []) of
	{ok, 200, _RespHeaders, ClientRef} ->
	    {ok, Body} = hackney:body(ClientRef),
	    Body;
	{ok, StatusCode, _RespHeaders, _ClientRef} ->
	    {error, {bad_status_code, StatusCode}};
	{error, Reason} ->
	    {error, Reason}
    end.


-spec parse_server_html(S :: binary()) -> list(binary()).
parse_server_html(S) ->
    [_| Splited] = binary:split(S, <<"https://">>, [global]),
    [get_server(X) || X <- Splited].

-spec get_server(S :: binary()) -> binary().
get_server(S) ->
    [Splited| _] = binary:split(S, <<"<">>, []),
    Splited.

-spec get_servers_list() -> {ok, list(binary())} | {error, any()}.
get_servers_list() ->
    case get_servers_html() of
	{error, Reason} ->
	    {error, {hackney_error, Reason}};
	Body ->
	    {ok, parse_server_html(Body)}
    end.
	
