%%%-------------------------------------------------------------------
%% @doc tc dispatcher.
%% @end
%%%-------------------------------------------------------------------

-module(tc_dispatcher).

-behaviour(gen_statem).

-export([start_link/1]).

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
    Fn = fun(E, Acc) -> queue:in(E, Acc) end,
    NewTasks = lists:foldl(Fn, queue:new(), get_servers_list()),
    NewNtask = queue:len(NewTasks),
    NewS = S#s{tasks=NewTasks, ntask=NewNtask},
    io:format("Scrap now~p~n",[NewTasks]),
    {next_state, collecting, NewS, [{state_timeout, 100, launch_collecting}]}.



%% COLLECTING STATE
collecting(info, {'DOWN', Ref, process, Pid, normal}, S) ->
    {next_state, waiting, S, [{state_timeout, 100, wait_until_daily}]};
collecting(info, {'DOWN', Ref, process, Pid, _Reason}, S) ->
    {next_state, waiting, S, [{state_timeout, 100, wait_until_daily}]}.

    


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
	    
    
%%% Request Servers Functions
    
get_servers_html() ->
    Url = "https://status.travian.com/",
    {ok, {{_, 200, _}, _, Body}} = httpc:request(get, {Url, []}, [], []),
    Body.

parse_server_html(S) ->
    [_| Splited] = string:split(S, "https://", all),
    [{get_server(X), 0} || X <- Splited].

get_server(S) ->
    [Splited| _] = string:split(S, "<", leading),
    Splited.

get_servers_list() ->
    parse_server_html(get_servers_html()).
