%%%--------------------------------------------------------------------- 
%%% Copyright Geng Li 2008
%%%---------------------------------------------------------------------

%%%--------------------------------------------------------------------- 
%%% Description module erlGP_utility
%%%--------------------------------------------------------------------- 
%%% erlGP_utility contains utility functions used by other modules
%%%--------------------------------------------------------------------- 
%%% Exports
%%%--------------------------------------------------------------------- 
%%% random_choice/1
%%% 	randomly choose a member from an input list
%%% num_of_possible_programs/2
%%%		calculates the number of possible programs for a given domain
%%%---------------------------------------------------------------------
-module(erlGP_utility).
-compile(export_all).

-include("erlGP_struct.hrl").

%%----------------------------------------------------------------------
%% Function: random_choice/1
%% Purpose: random choose one member from a list
%% Args:   List is A list()
%% Returns: a member randomly
%% Version: 1.0
%% Remarks: None
%%----------------------------------------------------------------------
random_choice(List) ->
	lists:nth(random:uniform(length(List)), List).

%%----------------------------------------------------------------------
%% Function: num_of_possible_programs/2
%% Purpose: calculates the number of possible programs for a given domain
%% Args:   Module is A atom()
%%		   MaxDepth is A int()
%% Returns: the number of programs with specified maxDepth in domain Module
%% Version: 1.0
%% Remarks: None
%%----------------------------------------------------------------------
num_of_possible_programs(Module, MaxDepth) ->
	num_of_possible_programs(Module, 1, MaxDepth).
num_of_possible_programs(Module, _MaxDepth, _MaxDepth) ->
	length(Module:terminals());
num_of_possible_programs(Module, Depth, MaxDepth) ->
	length(Module:terminals()) + lists:foldl(fun(Num, Sum) -> Num + Sum end, 0,
		lists:map(fun(#function_def{arity=Arity}) -> 
			Arity*num_of_possible_programs(Module, Depth + 1, MaxDepth) end, 
			Module:functions())).
	