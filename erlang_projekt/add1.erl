%% @doc Erlang mini project.
-module(add1).
-export([start/4, merge/3]).

    

%% @doc TODO: add documentation
-spec start(A,B,Base, Options) -> ok when 
      A::integer(),
      B::integer(), 
      Base::integer(),
      Option::atom() | tuple(),
      Options::[Option].

worker({ListA,ListB}, CollectPID, Base) ->
    receive
	{first, PID} ->
	    {ResultList, CarryList} = utils:add(ListA, ListB, Base),
	    CollectPID ! {1, {ResultList, CarryList}},
	    [CarryToSend|_] = CarryList,
	    PID ! {carry, CarryToSend};
	{alone} ->
	    Result = {_ResultList, _CarryList} = utils:add(ListA, ListB, Base),
	    CollectPID ! Result;
	{last, Pos} ->
	    receive 
		{carry, Carry} ->
		    {ResultList, CarryList} = utils:add(ListA, ListB, Base, Carry),
		    CollectPID ! {Pos, {ResultList, CarryList}}
			
	    end;
	{carry, Carry} ->
	    Result = {ResultList, CarryList} = utils:add(ListA, ListB, Base, Carry),
	    CollectPID ! Result,
	    receive
		{pid, PID, Pos} ->
		    [CarryToSend|_] = CarryList,
		    PID ! {carry, CarryToSend},
		    CollectPID ! {Pos, {ResultList, CarryList}}
	    end
    end.


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



collect(0, AuxList) ->
    merge(lists:keysort(1, AuxList),[],[]);
collect(N, AuxList) ->
    receive
	{Pos ,{ResList, CarList}} ->
	    collect(N-1, [{Pos, {ResList, CarList}} | AuxList]);
	{ResList, CarList} -> %% Alone
	    {ResList, CarList}
    end.

workerSpawner([],[], Aux, _, _) ->
    Aux;
workerSpawner([HeadA |ListA], [HeadB |ListB], Aux, CollectPID, Base) ->
    workerSpawner(ListA, ListB, [spawn_link(fun()-> worker({HeadA, HeadB}, CollectPID, Base) end) | Aux], CollectPID, Base).



start(A,B,Base, N) ->
    A1 = utils:integerToListBase(A, Base),
    B1 = utils:integerToListBase(B, Base),
    {A2, B2} = special:fill_list(A1, B1),
    %%A3 = lists:reverse(A2),
    %%B3 = lists:reverse(B2),
    SplitA = utils:split(A2, N),
    SplitB = utils:split(B2, N),
    CollectPID = self(),
    
    %%    WorkerPids = lists:reverse([spawn_link(fun() -> worker({ListA, ListB}, CollectPID, Base) end) || ListA <- SplitA , ListB <- SplitB]),
    WorkerPids = workerSpawner(SplitA, SplitB, [], CollectPID, Base),
    [FirstProcess|RestPids] = WorkerPids, 


    if
	length(WorkerPids) =:= 1 ->
	    FirstProcess ! {alone};
	true -> 
	    SecondProcess = lists:nth(2, WorkerPids),
	    FirstProcess ! {first, SecondProcess},
	    sendPIDs(RestPids)
    end,
    
    
    {Result, Carry} = collect(length(WorkerPids), []),
    %%io:format(Comb ++ "~n"),
    special:print_result(Carry, A2, B2, Result).

    

    
%% start with pos 2 cuz first already knows its first
sendPIDs(List) ->
    sendPIDs(List, 2).

sendPIDs(L, Pos) when length(L) =:= 1 ->
    [SendPID] = L,
    SendPID ! {last, Pos};

sendPIDs([SendPID|[RecievePID|L]], Pos) ->
    SendPID ! {pid, RecievePID, Pos},
    sendPIDs([RecievePID|L], Pos + 1).



%add(A,B,Base, Acc, 0) ->
%    add_(lists:reverse(A), lists:reverse(B), Base, Acc).
