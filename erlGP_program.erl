%%%--------------------------------------------------------------------- 
%%% Copyright Geng Li 2008
%%%---------------------------------------------------------------------

%%%--------------------------------------------------------------------- 
%%% Description module erlGP_program
%%%--------------------------------------------------------------------- 
%%% erlGP_program is the core data structure used in ErlGP. erlGP_program
%%%	module contains auxiliary functions to work on program structure
%%%--------------------------------------------------------------------- 
%%% Exports
%%%--------------------------------------------------------------------- 
%%% depth/1
%%%		computes the depth or a program tree
%%% generate_full/2
%%%		generates a program tree with specified depth basd on full
%%%		strategy
%%% generate_grow/2
%%%		generates a program tree with specified depth basd on grow
%%%		strategy
%%% get_node_at/2
%%%		retrives the node from a program tree at specified index
%%%	num_of_nodes/1
%%%		counts the number of nodes in a program tree 
%%% output_program/1
%%%		converts program into simpler form for output
%%%---------------------------------------------------------------------
-module(erlGP_program).
-compile(export_all).
%-export([depth/1]).
%-export([generate_full/2, generate_grow/2]).
%-export([get_node_at/2, num_of_nodes/1]).
%-export([output_program/1]).

-include("erlGP_struct.hrl").

%%----------------------------------------------------------------------
%% Function: depth/1
%% Purpose: computes the depth of a program
%% Args:   program() or atom()
%% Returns: depth of the program as int()
%% Version: 1.0
%% Remarks: None
%%----------------------------------------------------------------------
depth(#program{args=Args}) ->
	1 + lists:max(lists:map(fun depth/1, Args));
depth(_Terminal) -> 1.

%%----------------------------------------------------------------------
%% Function: generate_full/2
%% Purpose: generates a program tree with specified depth basd on full
%%			strategy
%% Args:   Module is A atom(), which stores the module name of a problem
%%		   domain
%% 		   MaxDepth is A int(), which is the depth of the generated tree
%% Returns: generated program of type program()
%% Version: 1.0
%% Remarks: uses generate_full/3 auxiliary function to generate tree
%%			recursively. This function is not tail recursive, generating
%%			program for big tree takes very long time
%%----------------------------------------------------------------------	
generate_full(Module, MaxDepth) ->
	generate_full(Module, 1, MaxDepth).
generate_full(Module, Depth, MaxDepth) ->
	if Depth >= MaxDepth ->
		erlGP_utility:random_choice(Module:terminals());
	true ->
		Func = erlGP_utility:random_choice(Module:functions()),
		Args = lists:map(fun(_Index) -> generate_full(Module, Depth + 1, MaxDepth) end,
			lists:seq(1, Func#function_def.arity)),
		#program{name=Func#function_def.name, func=Func#function_def.func, args=Args}
	end.

%%----------------------------------------------------------------------
%% Function: generate_grow/2
%% Purpose: generates a program tree with specified depth basd on grow
%%			strategy
%% Args:   Module is A atom(), which stores the module name of a problem
%%		   domain
%% 		   MaxDepth is A int(), which is the depth of the generated tree
%% Returns: generated program of type program()
%% Version: 1.0
%% Remarks: uses generate_grow/3 auxiliary function to generate tree
%%			recursively. This function is not tail recursive, generating
%%			program for big tree takes very long time. In addition, there
%%			is no unit testing for this function
%%----------------------------------------------------------------------
generate_grow(Module, MaxDepth) ->
	generate_grow(Module, 1, MaxDepth).
generate_grow(Module, Depth, MaxDepth) ->
	if Depth >= MaxDepth ->
		erlGP_utility:random_choice(Module:terminals());
	true ->
		if Depth < MaxDepth div 2 ->
			Type = function;
		true ->
			Type = erlGP_utility:random_choice([terminal, function])
		end,
		case Type of
			terminal ->
				erlGP_utility:random_choice(Module:terminals());
			function ->
				Func = erlGP_utility:random_choice(Module:functions()),
				Args = lists:map(fun(_Index) -> generate_grow(Module, Depth + 1, MaxDepth) end,
					lists:seq(1, Func#function_def.arity)),
				#program{name=Func#function_def.name, func=Func#function_def.func, args=Args}
		end
	end.
	
%%----------------------------------------------------------------------
%% Function: num_of_nodes/1
%% Purpose: counts the number of nodes in a program tree
%% Args:   Program is A program()
%% Returns: the number of nodes in the input program tree
%% Version: 1.0
%% Remarks: number_of_nodes/2 auxiliary function is used to recursively
%%			count the number of nodes
%%----------------------------------------------------------------------
num_of_nodes(#program{} = Program) -> 
	num_of_nodes(Program, 0);
num_of_nodes(_Terminal) -> 1.
num_of_nodes(#program{args=Args}, Count) ->
	num_of_nodes(Args, Count + 1);
num_of_nodes([], Count) -> Count;
num_of_nodes([Arg1 | Rest], Count) ->
	num_of_nodes(Rest, num_of_nodes(Arg1, Count));
num_of_nodes(_Terminal, Count) -> Count + 1.

%%----------------------------------------------------------------------
%% Function: get_node_at/2
%% Purpose: retrives the node from a program tree at specified index
%% Args:   Program is A program()
%%		   Index is A int()
%% Returns: the node at specified index
%% Version: 1.0
%% Remarks: None
%%----------------------------------------------------------------------
get_node_at(#program{} = Program, Index) ->
	{_, Result} = get_node_at(Program, Index, 0, {}),
	Result;
get_node_at(Terminal, 1) ->
	Terminal.
get_node_at(#program{args=Args} = Program, Index, Count, Result) ->
	if Index == Count + 1 -> {Count + 1, Program};
	true -> get_node_at(Args, Index, Count + 1, Result)
	end;
get_node_at([], _Index, Count, Result) -> {Count, Result};
get_node_at([Arg1 | Rest], Index, Count, Result) ->
	{Count2, Result2} = get_node_at(Arg1, Index, Count, Result),
	if Result2 == {} -> get_node_at(Rest, Index, Count2, Result2);
	true -> {Count2, Result2}
	end;
get_node_at(Terminal, Index, Count, Result) ->
	if Index == Count + 1 -> {Count + 1, Terminal};
	true -> {Count + 1, Result}
	end.

%%----------------------------------------------------------------------
%% Function: output_program/1
%% Purpose: converts program into simpler form for output
%% Args:   Program is A program()
%% Returns: a tuple represents program
%% Version: 1.0
%% Remarks: this function is not tail recursive
%%----------------------------------------------------------------------
output_program(#program{name=Name, args=Args}) ->
	{Name, output_program(Args, [])};
output_program(Terminal) -> Terminal.
output_program([Arg1 | Rest], Result) ->
	if Rest /= [] ->
		output_program(Rest, [output_program(Arg1) | Result]);
	true ->
		[output_program(Arg1) | Result] 
	end.

	
	