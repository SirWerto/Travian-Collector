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







-spec request_map(Url :: binary()) -> binary().
request_map(Url) ->
    {ok, 200, _RespHeaders, ClientRef}=hackney:request(get, Url, [], <<>>, []),
    {ok, Body} = hackney:body(ClientRef),
    Body.

-spec parse_binary_map(Map :: binary()) -> {ok, binary()}.
parse_binary_map(Map) ->
    Lines = binary:split(Map, [<<10>>], [global]), %% 10 == line jump
    Acc = <<"Grid,x,y,Troop_Id,Village_Id,Village_Name,Player_Id,Player_Name,Alliance_Id,Alliance_Name,Population,Territory",10>>,
    PMap = lists:foldl(fun parse_binary_line/2, Acc, Lines),
    {ok, PMap}.
    
-spec parse_binary_line(Line :: binary() | <<>>, Acc :: binary()) -> binary().
parse_binary_line(<<>>, Acc) ->
    Acc;
parse_binary_line(Line, Acc) ->
    %%30 start info
    %% last 7 bytes are useless
    Info = binary:part(Line, 30, byte_size(Line)-30-2),
    <<Acc/binary, Info/binary, 10>>.


-spec do_map(binary()) -> {ok, binary()}.
do_map(Url) ->
    ComposeUrl = <<"https://", Url/binary, "/map.sql">>,
    Body = request_map(ComposeUrl),
    {ok, _PMap} = parse_binary_map(Body).

    
    

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
    NameDay = [integer_to_list(Year), "-", integer_to_list(Month), "-", integer_to_list(Day), ".csv"],
    ok = file:write_file(Path++NameDay, binary_to_list(PMap)),
    ok.

    
    
