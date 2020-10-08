%%%-------------------------------------------------------------------
%% @doc tc worker.
%% @end
%%%-------------------------------------------------------------------

-module(tc_worker).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).


start_link() ->
    gen_server:start_link(?MODULE, []).

init([]) ->
    ok.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Request, State) ->
    {noreply, State}.
