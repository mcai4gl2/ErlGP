%%%--------------------------------------------------------------------- 
%%% Copyright Geng Li 2008
%%%--------------------------------------------------------------------- 

%%%--------------------------------------------------------------------- 
%%% Description module erlGP_breed
%%%--------------------------------------------------------------------- 
%%% erlGP_breed module contains all functions related to breeding
%%%--------------------------------------------------------------------- 
%%% Exports
%%%--------------------------------------------------------------------- 
%%% crossover/2
%%%		crossover two programs to produce two offsprings
%%% reproduce/1
%%%		reproduce a program
%%%---------------------------------------------------------------------
-module(erlGP_breed).
-compile(export_all).
%-export([crossover/2, reproduce/1]).

-include("erlGP_struct.hrl").

%%----------------------------------------------------------------------
%% Function: crossover/2
%% Purpose: crossover two programs to produce two offsprings
%% Args:   Program1 is A program()
%%		   Program2 is A program()
%% Returns: a list of two offsprings
%% Version: 1.0
%% Remarks: this function cannot be tested due to random factor used 
%%			within function. To test it, must export crossover/3 auxiliary
%%			function, so test function is not working when export_all is not
%%			comment out
%%----------------------------------------------------------------------
crossover(Program1, Program2) ->
	Length1 = erlGP_program:depth(Program1),
	Length2 = erlGP_program:depth(Program2),
	if Length1 == 1, Length2 == 1 ->
		[Program1, Program2];
	true ->
		Pos1 = choose_crossover_point(Program1),
		Pos2 = choose_crossover_point(Program2),
		if Pos1 == 1, Pos2 == 1 ->
			crossover(Program1, Program2);
		true ->
			[crossover(Program1, erlGP_program:get_node_at(Program2, Pos2), Pos1), 
		 	 crossover(Program2, erlGP_program:get_node_at(Program1, Pos1), Pos2)]
		end
	end.
crossover(Program, Replacement, Position) ->
	{_, Result} = crossover(Program, Replacement, Position,  0),
	Settings = erlGP_setting:get_settings(),
	Depth = erlGP_program:depth(Result),
	if Depth =< Settings#setting.max_crossover_depth -> Result;
	true -> {}
	end.
crossover(#program{name=Name, func=Func, args=Args}, Replacement, Position, Count) ->
	if Count + 1 == Position ->
		{Count + 1, Replacement};
	true ->
		{Count2, Result} = crossover(Args, Replacement, Position, Count + 1),
		{Count2, #program{name=Name, func=Func, args=Result}}
	end;
crossover([Arg1 | Rest], Replacement, Position, Count) ->
	{Count2, Replaced} = crossover(Arg1, Replacement, Position, Count),
	{Count3, Replaced2} = crossover(Rest, Replacement, Position, Count2),
	{Count3, [Replaced|Replaced2]};
crossover([], _Replacement, _Position, Count) ->
	{Count, []};
crossover(Terminal, Replacement, Position, Count) ->
	if Count + 1 == Position -> {Count + 1, Replacement};
	true -> {Count + 1, Terminal}
	end.

get_crossover_point(Program, BlackList, NumOfNodes, IsTerminal) ->
	Nodes = lists:seq(1, NumOfNodes),
	Choices = lists:dropwhile(fun(X) -> lists:any(fun(Y) -> Y == X end, BlackList) end, Nodes),
	Pos = erlGP_utility:random_choice(Choices),
	Node = erlGP_program:get_node_at(Program, Pos),
	if is_atom(Node) == IsTerminal -> Pos;
	true ->
		get_crossover_point(Program, [Pos | BlackList], NumOfNodes, IsTerminal)
	end.

choose_crossover_point(Program) ->
	Val = random:uniform(),
	if is_atom(Program) -> 1;
	true ->
		if Val >= 0.9 -> 
			get_crossover_point(Program, [], erlGP_program:num_of_nodes(Program), true);
		true ->
			get_crossover_point(Program, [], erlGP_program:num_of_nodes(Program), false)
		end
	end.

%%----------------------------------------------------------------------
%% Function: reproduce/1
%% Purpose: reproduces a program
%% Args:   Program1 is A program()
%% Returns: program()
%% Version: 1.0
%% Remarks: None
%%----------------------------------------------------------------------
reproduce(Program) ->
	Program.