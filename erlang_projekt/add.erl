%% @doc Erlang mini project.
-module(add).
-export([start/3, start/4]).

%% @doc TODO: add documentation
-spec start(A,B,Base) -> ok when 
      A::integer(),
      B::integer(), 
      Base::integer().

start(A,B, Base) when Base >= 2 andalso Base =< 32 ->
    

%% @doc TODO: add documentation
-spec start(A,B,Base, Options) -> ok when 
      A::integer(),
      B::integer(), 
      Base::integer(),
      Option::atom() | tuple(),
      Options::[Option].

start(A,B,Base, Options) ->
    tbi.






%add_([A1|A],[B1|B], Base, Acc, Carry) when Base =< 10 ->
%    if
%	A1 + B1 > Base -> 
%	    NewCarry = 1, 
%	    Result = A1 + B1 + Carry - Base;
%	true -> 
%	    NewCarry = 0,
%	    Result = A1 + B1 + Carry
%    end.
%
%
%add_(A,B, Base, Result, NewCarry).
%
%add_([A1|A],[B1|B], Base, Acc, Carry) when Base =< 32 ->
%    
%





%add(A,B,Base, Acc, 0) ->
%    add_(lists:reverse(A), lists:reverse(B), Base, Acc).
