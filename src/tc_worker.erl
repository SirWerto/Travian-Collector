%%%-------------------------------------------------------------------
%% @doc tc worker.
%% @end
%%%-------------------------------------------------------------------

-module(tc_worker).

-behaviour(gen_server).

-export([start_link/2, do_server/1, store_server/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).


start_link(Url, TDir) ->
    gen_server:start_link(?MODULE, [Url, TDir], []).

init([Url, TDir]) ->
    self() ! {scrap, Url, TDir},
    {ok, []}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({scrap, Url, TDir}, _State) ->
    {ok, {MapPhP, PMap}}=tc_worker:do_server(Url),
    ok = tc_worker:store_server(TDir, MapPhP, PMap),
    {stop, normal, {Url, TDir}};
handle_info(_Request, State) ->
    {noreply, State}.

terminate(normal, Url) ->
    {ok, Url}.







request_map(Url) ->
    {ok, {{_, 200, "OK"}, _, Body}} = httpc:request(Url),
    list_to_binary(Body).

parse_binary_map(Map) ->
    Lines = binary:split(Map, [<<10>>], [global]), %% 10 == line jump
    Acc = <<"Grid,x,y,Troop_Id,Village_Id,Village_Name,Player_Id,Player_Name,Alliance_Id,Alliance_Name,Population,Territory",10>>,
    PMap = lists:foldl(fun parse_binary_line/2, Acc, Lines),
    {ok, PMap}.
    
parse_binary_line(<<>>, Acc) ->
    Acc;
parse_binary_line(Line, Acc) ->
    %%30 start info
    %% last 7 bytes are useless
    Info = binary:part(Line, 30, byte_size(Line)-30-2),
    <<Acc/binary, Info/binary, 10>>.


do_map(Url) ->
    ComposeUrl = "https://" ++ Url ++ "/map.sql",
    Body = request_map(ComposeUrl),
    {ok, _PMap} = parse_binary_map(Body).
    
    

%%%----------Scrap Login----------


request_php(Url) ->
    {ok, {{_, 200, "OK"}, _, Body}} = httpc:request(Url),
    list_to_binary(Body).

parse_binary_php(BPhp) ->
    [_, TempWID] = binary:split(BPhp, <<"Travian.Game.worldId = '">>),
    [WorldID , _] = binary:split(TempWID, <<"'">>),
    MapPhP = #{
	      wid => binary:bin_to_list(WorldID)
	      },
    {ok, MapPhP}.
    
do_php(Url) ->
    ComposeUrl = "https://" ++ Url ++ "/login.php",
    Body = request_php(ComposeUrl),
    {ok, MapPhP} = parse_binary_php(Body),
    {ok, maps:put(name, maps:get(wid, MapPhP) ++ "-" ++ Url, MapPhP)}.


%%%----------Interfaze with us-------
do_server(Url) ->
    {ok, MapPhP} = do_php(Url),
    {ok, PMap} = do_map(Url),
    {ok, {MapPhP, PMap}}.

store_server(TDir, MapPhP, PMap) ->
    Path = TDir++"/"++maps:get(name,MapPhP)++"/",
    ok = filelib:ensure_dir(Path),
    io:format("~p~n", [Path]),
    NameInfo = ["info", ".txt"],
    %ok = file:write_file(Path++NameInfo, maps:to_list(MapPhP)),
    {{Year,Month,Day}, _} = erlang:localtime(),
    NameDay = [integer_to_list(Year), "-", integer_to_list(Month), "-", integer_to_list(Day), ".csv"],
    io:format("~p~n", [NameDay]),
    ok = file:write_file(Path++NameDay, binary_to_list(PMap)),
    ok.

    
    
