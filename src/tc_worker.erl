%%%-------------------------------------------------------------------
%% @doc tc worker.
%% @end
%%%-------------------------------------------------------------------

-module(tc_worker).

-behaviour(gen_server).

-export([start_link/2, do_server/1, store_server/3, extract_line/1]).

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







-spec request_map(Url :: binary()) -> binary().
request_map(Url) ->
    {ok, 200, _RespHeaders, ClientRef}=hackney:request(get, Url, [], <<>>, []),
    {ok, Body} = hackney:body(ClientRef),
    Body.

-spec parse_binary_map(Map :: binary()) -> {ok, binary()}.
parse_binary_map(Map) ->
    Lines = binary:split(Map, [<<10>>], [global]), %% 10 == line jump
    Acc = <<"[",10>>,
    PMap = lists:foldl(fun parse_binary_line/2, Acc, Lines),
    JMap = binary:part(PMap, {0,byte_size(PMap) -2}),
    {ok, <<JMap/binary, "]">>}.
    
-spec parse_binary_line(Line :: binary() | <<>>, Acc :: binary()) -> binary().
parse_binary_line(<<>>, Acc) ->
    Acc;
parse_binary_line(Line, Acc) ->
    %%30 start info
    %% last 7 bytes are useless
    PLine = binary:part(Line, 30, byte_size(Line)-30-2),
    Json = extract_line(PLine),
    <<Acc/binary, Json/binary, ",", 10>>.


-spec do_map(binary()) -> {ok, binary()}.
do_map(Url) ->
    ComposeUrl = <<"https://", Url/binary, "/map.sql">>,
    Body = request_map(ComposeUrl),
    {ok, _PMap} = parse_binary_map(Body).

-spec extract_line(Line :: binary()) -> binary().
extract_line(<<>>) ->
    <<>>;
extract_line(Line) ->
    case length(binary:matches(Line, <<",">>)) of
	11 ->
	    PLine = handle_easy_line(Line),
	    tojson(PLine);
	_ ->
	    <<"{}">>
	    %PLine = handle_bad_line(Line),
	    %tojson(PLine)
    end.

-spec handle_easy_line(Line :: binary()) -> list().
handle_easy_line(Line) ->
     binary:split(binary:replace(Line, <<"'">>, <<>>, [global]), <<",">>, [global]).

-spec handle_bad_line(Line :: binary()) -> list().
handle_bad_line(Line) ->
    [Grid, T1] = binary:split(Line, <<",">>),
    [X, T2] = binary:split(T1, <<",">>),
    [Y, T3] = binary:split(T2, <<",">>),
    [Troop, T4] = binary:split(T3, <<",">>),
    case length(binary:matches(T4, <<",">>)) of
	7 ->
	    ok;
	_ ->
	    ok
    end.

-spec tojson(List :: list()) -> binary().
tojson([Grid,X,Y,TroopId,VillageId,VillageName,PlayerId,PlayerName,AllianceId,AllianceName,Population,Territory]) ->
    Tocode = {[
	      {<<"grid">> , Grid},
	      {<<"x">> , X},
	      {<<"y">> , Y},
	      {<<"troopid">> , TroopId},
	      {<<"villageid">> , VillageId},
	      {<<"villagename">> , VillageName},
	      {<<"playerid">> , PlayerId},
	      {<<"playername">> , PlayerName},
	      {<<"allianceid">> , AllianceId},
	      {<<"alliancename">> , AllianceName},
	      {<<"population">> , Population},
	      {<<"territory">> , Territory}
	     ]},
    jiffy:encode(Tocode).
	    
    



    

%%%----------Scrap Login----------


-spec request_php(Url :: binary()) -> binary().
request_php(Url) ->
    {ok, 200, _RespHeaders, ClientRef}=hackney:request(get, Url, [], <<>>, []),
    {ok, Body} = hackney:body(ClientRef),
    Body.

-spec parse_binary_php(BPhp :: binary()) -> {ok, map()}.
parse_binary_php(BPhp) ->
    [_, TempAppId] = binary:split(BPhp, <<"Travian.applicationId = '">>),
    [AppId , _] = binary:split(TempAppId, <<"'">>),
    [_, TempVersion] = binary:split(BPhp, <<"Travian.Game.version = '">>),
    [Version , _] = binary:split(TempVersion, <<"'">>),
    [_, TempWID] = binary:split(BPhp, <<"Travian.Game.worldId = '">>),
    [WorldID , _] = binary:split(TempWID, <<"'">>),
    [_, TempSpeed] = binary:split(BPhp, <<"Travian.Game.speed = ">>),
    [Speed , _] = binary:split(TempSpeed, <<";">>),
    [_, TempCountry] = binary:split(BPhp, <<"Travian.Game.country = '">>),
    [Country , _] = binary:split(TempCountry, <<"'">>),
    MapPhP = #{
	      appid => binary:bin_to_list(AppId),
	      version => binary:bin_to_list(Version),
	      wid => binary:bin_to_list(WorldID),
	      speed => erlang:binary_to_integer(Speed, 10),
	      country => binary:bin_to_list(Country)
	      },
    {ok, MapPhP}.
    
-spec do_php(Url :: binary()) -> {ok, map()}.
do_php(Url) ->
    ComposeUrl = <<"https://",Url/binary ,"/login.php">>,
    Body = request_php(ComposeUrl),
    {ok, MapPhP} = parse_binary_php(Body),
    {ok, maps:put(name, maps:get(wid, MapPhP) ++ "-" ++ erlang:binary_to_list(Url), MapPhP)}.


%%%----------Interfaze with us-------
-spec do_server(Url :: binary()) -> {ok, {map(), binary()}}.
do_server(Url) ->
    {ok, MapPhP} = do_php(Url),
    {ok, PMap} = do_map(Url),
    {ok, {MapPhP, PMap}}.

-spec store_server(TDir :: string(), MapPhP :: map(), PMap :: binary()) -> ok.
store_server(TDir, MapPhP, PMap) ->
    Path = TDir++"/"++maps:get(name,MapPhP)++"/",
    ok = filelib:ensure_dir(Path),
    NameInfo = ["info", ".txt"],
    %ok = file:write_file(Path++NameInfo, maps:to_list(MapPhP)),
    {{Year,Month,Day}, _} = erlang:localtime(),
    NameDay = [integer_to_list(Year), "-", integer_to_list(Month), "-", integer_to_list(Day), ".json"],
    ok = file:write_file(Path++NameDay, binary_to_list(PMap)),
    ok.

    
    
