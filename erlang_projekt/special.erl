%% @doc Functions for add module
-module(special).
-export([]).

%% To use EUnit we must include this.
-include_lib("eunit/include/eunit.hrl").

-compile(export_all). 


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
