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
	    monitors=maps:new(),
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



scraping(info, {start_sup, Parent}, _S) ->
    {ok, Sup} = supervisor:start_child(Parent, ?SPEC),
    {ok, Tries} = application:get_env(maxTries),
    {ok, Workers} = application:get_env(maxWorkers),
    {ok, Daily} = application:get_env(daily),
    Hour = maps:get(hour, Daily),
    Minute = maps:get(minute, Daily),
    Second = maps:get(second, Daily),
    S = #s{supervisor=Sup,
	   maxTries = Tries,
	   maxWorkers = Workers,
	   daily = {Hour, Minute, Second}},
    {next_state, scraping, S, [{state_timeout, 10000, scraping}]};

scraping(state_timeout, _State, S) ->
    io:format("Do something cool~p~n",[S]),
    {next_state, collecting, S, [{state_timeout, 1, scraping}]}.


collecting(state_timeout, scraping, S) ->
    io:format("Do something cool, but in collecting~p~n",[S]),
    {next_state, waiting, S, [{state_timeout, 1, waiting}]}.

    


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
    {next_state, scraping, S, [{state_timeout, 10000, scraping}]};

waiting(state_timeout, _State, S) ->
    io:format("Wait until daily launch~n"),
    {next_state, scraping, S, [{state_timeout, 2000, scraping}]}.


    

    
    
