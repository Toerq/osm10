%% @doc Erlang mini project.
-module(add).
-export([start/3, start/4]).

%% To use EUnit we must include this.
-include_lib("eunit/include/eunit.hrl").

%% @doc TODO: add documentation
-spec start(A,B,Base) -> ok when 
      A::integer(),
      B::integer(), 
      Base::integer().

start(A,B, Base) when Base >= 2 andalso Base =< 36 ->
    {Al,Bl} = special:fill_list(utils:integerToListBase(A,Base),utils:integerToListBase(B,Base)),
    {Result, Carry} = utils:add(Al,Bl,Base),
    special:print_result(Carry, Al, Bl, Result).
    

%% @doc TODO: add documentation
-spec start(A,B,Base, Options) -> ok when 
      A::integer(),
      B::integer(), 
      Base::integer(),
      Option::atom() | tuple(),
      Options::[Option].

start(A,B,Base, Options) ->
    tbi.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                          %%
%%			   EUnit Test Cases                                 %%
%%                                                                          %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		     
    
result_test_() ->
    Assert = fun(A,B,Base) ->
		     {Al,Bl} =  special:fill_list(utils:integerToListBase(A,Base),utils:integerToListBase(B,Base)),
		     {Result,_Carry} = utils:add(Al,Bl,Base),
		     ?_assertEqual(A+B,list_to_integer(special:int_list_to_string(Result),Base))
	     end, 
    random:seed(erlang:now()),
    [Assert(A,B,Base) || A <- [random:uniform(1000000) || _ <- lists:seq(1,8)],
			      B <- [random:uniform(1000000) || _ <- lists:seq(1,8)],
			      Base <- lists:seq(2,36)].
