%%%-------------------------------------------------------------------
%% @doc tc top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(tc_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    
    Dispatcher = #{id => dispatcher,
		   start => {tc_dispatcher, start_link, [self()]},
		   restart => permanent,
		   shutdown => 10000,
		   type => worker,
		   modules => [tc_dispatcher]},

    ChildSpecs = [Dispatcher],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
