-module(prop_tc_dispatcher).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_test() ->
    ?FORALL(Type, mytype(),
        begin
            boolean(Type)
        end).

prop_add_one_day() ->
    ?FORALL(Ttuple, {non_neg_integer(), range(1,12), range(1,31)},
    ?IMPLIES(no_bad_days_february(Ttuple),
        begin
	    1 =:= calendar:date_to_gregorian_days(tc_dispatcher:add_one_day(Ttuple)) -
		  calendar:date_to_gregorian_days(Ttuple)
        end)).

prop_time_difference() ->
    ?FORALL(Otime, {range(0,23), range(0,59), range(0,59)},
        begin
	    Datetime = calendar:local_time(),
	    calendar:datetime_to_gregorian_seconds(future(Otime, Datetime)) 
		=:= tc_dispatcher:time_difference(Otime, Datetime)
		+ calendar:datetime_to_gregorian_seconds(Datetime)
        end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
boolean(_) -> true.

future(Otime, {Date, _Time}) ->
    {tc_dispatcher:add_one_day(Date), Otime}.

no_bad_days_february({Year, 2, Day}) ->
    case calendar:is_leap_year(Year) of
	true ->
	    Day =< 29;
	false ->
	    Day =< 28
    end;
no_bad_days_february({Year, Month, Day}) ->
    case Month of
	4 -> Day =< 30;
	6 -> Day =< 30;
	9 -> Day =< 30;
	11 -> Day =< 30;
	_ -> true
    end.

    
    

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%
mytype() -> term().
