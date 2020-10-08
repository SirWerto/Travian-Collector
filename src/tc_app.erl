%%%-------------------------------------------------------------------
%% @doc tc public API
%% @end
%%%-------------------------------------------------------------------

-module(tc_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    tc_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
