%% @doc Functions for add module
-module(special).
-export([]).

%% To use EUnit we must include this.
-include_lib("eunit/include/eunit.hrl").

-compile(export_all). 


line(N) ->
    utils:repeat($-, N).

remove_all_zeros(List) ->
    F = fun(X) when X =:= 48 -> X-16; (X) -> X  end,
    lists:map(F,List).

create_carry_line(List) ->
    F = fun(X) when X =:= 1 -> 45; (_X) -> 32  end,
    lists:map(F,List).

remove_first_zeros([]) ->
    [];
remove_first_zeros([48|T]) ->
    [32|remove_first_zeros(T)];
remove_first_zeros(List) ->
    List.
    
int_list_to_string(List) ->
    F = fun(X) when X < 10 -> X+48; (X) -> X+55 end,
    lists:map(F,List).
		

print_result(Carry, A, B, Result) ->
    io:format("  " ++ remove_all_zeros(int_list_to_string(Carry))++"~n"),
    io:format("  " ++ create_carry_line(Carry)++"~n"),
    io:format("   " ++ remove_first_zeros(int_list_to_string(A))++"~n"),
    io:format("   " ++ remove_first_zeros(int_list_to_string(B))++"~n"),
    io:format("+ " ++ line(length(Result))++"~n"),
    io:format("  " ++ remove_first_zeros(int_list_to_string(Result))++"~n")
    .

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Adds Difference zeros to List
add_zero(List, 0) ->
    List;
add_zero(List, Difference) ->
    add_zero([0|List], Difference-1).

%% @doc If the lists L1 and L2 are of different lengths, add zeros
%% to the shorter list so that they are of the _same length_.
fill_list(L1, L2) ->
    Length1 = erlang:length(L1),
    Length2 = erlang:length(L2),
    Difference = Length1 - Length2,
    if
	Difference > 0 ->
	    {L1, add_zero(L2, Difference)};
	true ->
	    {add_zero(L1, -Difference), L2}
    end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
