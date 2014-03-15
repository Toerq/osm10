%% @doc Functions for add module
-module(special).
-export([fill_list/2, print_result/4, add/3, add/4, 
	 integerToListBase/2, workerSpawner/5, sendPIDs/1, collect/2]).

%% To use EUnit we must include this.
-include_lib("eunit/include/eunit.hrl").

%%-compile(export_all). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Adds Difference zeros to List
add_zero(List, 0) ->
    List;
add_zero(List, Difference) ->
    add_zero([0|List], Difference-1).

%% @doc If the lists List1 and List2 are of different lengths, add zeros
%% to the shorter list so that the resulting ListP and ListQ are of
%% equal length.
%% <div class="example">```
%% fill_list([1,2,3,4,5,6,7],[1,2,3]).
%% {[1,2,3,4,5,6,7],[0,0,0,0,1,2,3]}'''
%% </div>
-spec fill_list(List1, List2) -> {ListP, ListQ} when
      List1 :: [integer()],
      List2 :: [integer()],
      ListP :: [integer()],
      ListQ :: [integer()].

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
		
%% @doc Prints Carry, A,B and Result.
%% === Example ===
%% <div class="example">```
%% print_result([0,1,0,0],[2,9,1,0],[3,5,0,0],[6,4,1,0]).
%%    1  
%%    -  
%%    2910
%%    3500
%% + ----
%%    6410
%% ok'''
%% </div>
-spec print_result(Carry,A,B,Result) -> ok when
      Carry::[integer()],
      A::[integer()],
      B::[integer()],
      Result::[integer()].
      
print_result(Carry, A, B, Result) ->
    io:format("  " ++ remove_all_zeros(int_list_to_string(Carry))++"~n"),
    io:format("  " ++ create_carry_line(Carry)++"~n"),
    io:format("   " ++ remove_first_zeros(int_list_to_string(A))++"~n"),
    io:format("   " ++ remove_first_zeros(int_list_to_string(B))++"~n"),
    io:format("+ " ++ line(length(Result))++"~n"),
    io:format("  " ++ remove_first_zeros(int_list_to_string(Result))++"~n").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Creates a list of '-' characters with size N.
line(N) ->
    utils:repeat($-, N).

%% Replaces all zeros in List with spaces.
remove_all_zeros(List) ->
    F = fun(X) when X =:= 48 -> X-16; (X) -> X  end,
    lists:map(F,List).

%% Copy List but replaces all 1:s with 45 (ASCII for '-'). 
create_carry_line(List) ->
    F = fun(X) when X =:= 1 -> 45; (_X) -> 32  end,
    lists:map(F,List).

%% Replaces the zeros in the beginning of List with spaces.
remove_first_zeros([]) ->
    [];
remove_first_zeros([48|T]) ->
    [32|remove_first_zeros(T)];
remove_first_zeros(List) ->
    List.

%% Converts a list with integers to corresponding string    
int_list_to_string(List) ->
    F = fun(X) when X < 10 -> X+48; (X) -> X+55 end,
    lists:map(F,List).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Returns {NewList, CarrierList} where NewList is 
%% the result of A added to B in base Base. CarrierList 
%% contains a list of every of 1's and 0's where the 1's 
%% represents if a carry in has been made and 0 if a
%% carry in has not been made.
%%
%% === Example ===
%% <div class="example">```
%% 1> utils:add([1,0,1],[1,1,1], 2).
%% {[1,1,0,0],[1,1,1,0]}'''
%% </div>
-spec add(A, B, Base) -> {NewList, CarrierList} when
      A :: [List],
      B :: [List],
      Base :: integer(),
%%    lists:length(A) =:= lists:length(B),
      NewList :: [List],
      CarrierList :: [List].

add(A, B, Base) ->
    add(lists:reverse(A), lists:reverse(B), Base, 0, [], []).

add(A, B, Base, Carry) ->
    add(lists:reverse(A), lists:reverse(B), Base, Carry, [], []).

add([],[], _, Carry, ResultList, CarrierList) ->
    {[Carry|ResultList],  [Carry|CarrierList]};

add([A1|A], [B1|B], Base, Carry, ResultList, CarrierList) ->
    NewCarry = (A1 + B1 + Carry) div Base,
    NewResult = (A1 + B1 + Carry) rem Base,
    add(A, B, Base, NewCarry, [NewResult|ResultList], [Carry|CarrierList]).

%% @doc Returns List where List is a repreentation of Number
%% in Base divided into elements.
%%
%% === Example ===
%% 
%% <div class="example">```
%% 1> utils:integerToListBase(127,3).
%% [1,1,2,0,1]'''
%% </div>
integerToListBase(Number, Base) ->
    integerToListBase(Number, Base, []).

integerToListBase(0 , _Base, List) ->
    List;
integerToListBase(Number, Base, List) ->
    integerToListBase(Number div Base ,Base , [Number rem Base | List]).

%% @doc TODO: add documentation
worker({ListA,ListB}, CollectPID, Base) ->
    receive
	{first, PID} ->
	    {ResultList, CarryList} = add(ListA, ListB, Base),
	    CollectPID ! {1, {ResultList, CarryList}},
	    [CarryToSend|_] = CarryList,
	    PID ! {carry, CarryToSend};
	{alone} ->
	    Result = {_ResultList, _CarryList} = add(ListA, ListB, Base),
	    CollectPID ! Result;
	{last, Pos} ->
	    receive 
		{carry, Carry} ->
		    {ResultList, CarryList} = add(ListA, ListB, Base, Carry),
		    CollectPID ! {Pos, {ResultList, CarryList}}
			
	    end;
	{carry, Carry} ->
	    %%Result = 
	    {ResultList, CarryList} = add(ListA, ListB, Base, Carry),
	    %%CollectPID ! Result,
	    receive
		{pid, PID, Pos} ->
		    [CarryToSend|_] = CarryList,
		    PID ! {carry, CarryToSend},
		    CollectPID ! {Pos, {ResultList, CarryList}}
	    end
    end.

%% TODO: add documentation
%% ANTAGLIGEN FEL HÄR!!!
merge([L], ResultList, CarryList) ->
    %% we do care about the head in the base case, 
    %% because this is the (msb)carryover.
    {_,{Result, Carry}} = L, 
    {Result ++ ResultList, Carry ++ CarryList};
merge([L1|L],ResultList, CarryList) ->
    %% the head is the carryover so when we merge we dont want it here
    {_,{[_  | Result], [_ | Carry]}} = L1, 
    merge(L, Result ++ ResultList, Carry ++ CarryList).

%% TODO: add documentation
collect(0, AuxList) ->
    io:format("~p~n~n", [AuxList]),
    merge(lists:keysort(1, AuxList),[],[]);
collect(N, AuxList) ->
    receive
	{Pos ,{ResList, CarList}} ->
	    collect(N-1, [{Pos, {ResList, CarList}} | AuxList]);
	{ResList, CarList} -> %% Alone
	    {ResList, CarList}
    end.

%% TODO: add documentation
workerSpawner([],[], Aux, _, _) ->
    Aux;
workerSpawner([HeadA |ListA], [HeadB |ListB], Aux, CollectPID, Base) ->
    workerSpawner(ListA, ListB, [spawn_link(fun()-> worker({HeadA, HeadB}, CollectPID, Base) end) | Aux], CollectPID, Base).

%% start with pos 2 cuz first already knows its first
sendPIDs(List) ->
    sendPIDs(List, 2).

sendPIDs(L, Pos) when length(L) =:= 1 ->
    [SendPID] = L,
    SendPID ! {last, Pos};

sendPIDs([SendPID|[RecievePID|L]], Pos) ->
    SendPID ! {pid, RecievePID, Pos},
    sendPIDs([RecievePID|L], Pos + 1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                          %%
%%			   EUnit Test Cases                                 %%
%%                                                                          %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

remove_all_zeros_test_() ->
    ?_assertEqual({" 1 2 3 ", "  1  9  "}, 
		  {remove_all_zeros("0102030"), 
		  remove_all_zeros("00100900")}).

remove_first_zeros_test_() ->
    ?_assertEqual({"  12030", "123"}, 
		  {remove_first_zeros("0012030"),
		  remove_first_zeros("123")}).

int_list_to_string_test_() ->
    ?_assertEqual({"98710", "00123"}, 
		  {int_list_to_string([9,8,7,1,0]), 
		   int_list_to_string([0,0,1,2,3])}).

add_zero_test_() ->
    ?_assertEqual({[0,0,0,1], [0,0,5,2,9]}, 
		  {add_zero([1],3), 
		   add_zero([5,2,9],2)}).

fill_list_test_() ->
    ?_assertEqual({{[1,2,3],[0,0,1]},
		   {[0,0,0,0,1,2,3,4,5,6],[9,8,7,6,5,4,3,2,1,0]}}, 
		  {fill_list([1,2,3],[1]),
		   fill_list([1,2,3,4,5,6],[9,8,7,6,5,4,3,2,1,0])}).

result_test_() ->
    Assert = fun(A,B,Base) ->
		     {Al,Bl} = fill_list(integerToListBase(A,Base),integerToListBase(B,Base)),
		     {Result,_Carry} = add(Al,Bl,Base),
		     ?_assertEqual(A+B,list_to_integer(int_list_to_string(Result),Base))
	     end, 
    random:seed(erlang:now()),
    [Assert(A,B,Base) || A <- [random:uniform(1000000) || _ <- lists:seq(1,8)],
			      B <- [random:uniform(1000000) || _ <- lists:seq(1,8)],
			      Base <- lists:seq(2,36)].
