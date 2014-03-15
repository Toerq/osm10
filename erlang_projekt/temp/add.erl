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
    {Al,Bl} = special:fill_list(special:integerToListBase(A,Base),
				special:integerToListBase(B,Base)),
    {Result, Carry} = special:add(Al,Bl,Base),
    special:print_result(Carry, Al, Bl, Result).
    

%% @doc TODO: add documentation
-spec start(A,B,Base, Options) -> ok when 
      A::integer(),
      B::integer(), 
      Base::integer(),
      Option::atom() | tuple(),
      Options::[Option].

start(A,B,Base,N) ->
    A1 = special:integerToListBase(A, Base),
    B1 = special:integerToListBase(B, Base),
    {A2, B2} = special:fill_list(A1, B1),
    SplitA = utils:split(A2, N),
    SplitB = utils:split(B2, N),
    CollectPID = self(),

    WorkerPids = special:workerSpawner(SplitA, SplitB, [], 
				       CollectPID, Base),
    
    io:format("~p~n", [WorkerPids]),
    [FirstProcess|RestPids] = WorkerPids, 

    if
	length(WorkerPids) =:= 1 ->
	    FirstProcess ! {alone};
	true -> 
	    SecondProcess = lists:nth(2, WorkerPids),
	    FirstProcess ! {first, SecondProcess},
	    special:sendPIDs(RestPids)
    end,
    
    
    {Result, Carry} = special:collect(length(WorkerPids), []),
    special:print_result(Carry, A2, B2, Result).

