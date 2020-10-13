%%%-------------------------------------------------------------------
%% @doc tc worker.
%% @end
%%%-------------------------------------------------------------------

-module(tc_worker).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).


start_link(Url) ->
    gen_server:start_link(?MODULE, [Url], []).

init([Url]) ->
    {ok, Url}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Request, State) ->
    {noreply, State}.

terminate(normal, Url) ->
    {ok, Url}.
