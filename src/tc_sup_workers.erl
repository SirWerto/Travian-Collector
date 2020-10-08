%%%-------------------------------------------------------------------
%% @doc tc worker level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(tc_sup_workers).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link(?MODULE, []).

init([]) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 20,
                 period => 10000},
    
    Worker = #{id => worker,
	       start => {tc_worker, start_link, []},
	       restart => temporary,
	       shutdown => 10000,
	       type => worker,
	       modules => [tc_worker]},

    ChildSpecs = [Worker],
    {ok, {SupFlags, ChildSpecs}}.
