%%%--------------------------------------------------------------------- 
%%% Copyright Geng Li 2008
%%%---------------------------------------------------------------------

%%%--------------------------------------------------------------------- 
%%% Description module erlGP_core
%%%--------------------------------------------------------------------- 
%%% erlGP_core includes all core functions in genetic programming
%%%--------------------------------------------------------------------- 
%%% Exports
%%%--------------------------------------------------------------------- 
%%% evaluate/2
%%%		evalautes the result of a program basd on input values
%%% generate_population/1
%%%		generates a population of programs from a domain
%%% evaluate_population_single/3
%%% 	evaluates population raw fitness using single thread mode
%%% evaluate_population_multi/3
%%%		evaluates population raw fitness using multi-threads mode
%%% evolve_population/2
%%% 	evolves evaluated population to produce offsprings
%%%---------------------------------------------------------------------
-module(erlGP_core).
-compile(export_all).

-include("erlGP_struct.hrl").

%%----------------------------------------------------------------------
%% Function: evaluate/2
%% Purpose: evalautes the result of a program
%% Args:   Module as atom()
%%		   Variables as [input()]
%%		   Program as program() or term()
%% Returns: the result of program based on input variables
%% Version: 1.0
%% Remarks: None
%%----------------------------------------------------------------------
evaluate(Variables, #program{func=Func, args=Args}) ->
	erlang:apply(Func, [Variables, Args]);
evaluate(Variables, Terminal) ->
	{value, Record} = lists:keysearch(Terminal, #input.name, Variables),
	Record#input.value.

%%----------------------------------------------------------------------
%% Function: generate_population/1
%% Purpose: generates a population of programs from a domain
%% Args:   Module as atom()
%% Returns: generated population as [program()]
%% Version: 1.0
%% Remarks: strategy used to generate population is ramp-half-and-half 
%%			based on Koza. There is no repeat program in population
%%----------------------------------------------------------------------
generate_population(Module) ->
	Settings = erlGP_setting:get_settings(),
	generate_population(Module, Settings#setting.population_size).
generate_population(Module, Size) ->
	Settings = erlGP_setting:get_settings(),
	MaxInitDepth = Settings#setting.max_init_depth,
	SizeLimit = lists:map(fun(Index) -> erlGP_utility:num_of_possible_programs(Module, Index) end,
		lists:seq(1, MaxInitDepth)),
	PopulationStruct = lists:map(
		fun(Index) -> {lists:nth(Index + 1, [0 | SizeLimit]) - lists:nth(Index, [0 | SizeLimit]), []} end,
		lists:seq(1, MaxInitDepth)),
 	generate_population(Module, Size, PopulationStruct, MaxInitDepth, Size).
generate_population(_Module, 0, Population, _MaxDepth, _Size) ->
	lists:foldl(fun({_, Individuals}, Acc) -> lists:append(Acc, Individuals) end, [], Population);
generate_population(Module, Index, Population, MaxDepth, Size) ->
	Settings = erlGP_setting:get_settings(),
	MaxInitDepth = Settings#setting.max_init_depth,
	if Index rem 2 == 0 ->
		Program = erlGP_program:generate_grow(Module, MaxDepth);
	true ->
		Program = erlGP_program:generate_full(Module, MaxDepth)
	end,
	Depth = erlGP_program:depth(Program),
	{Limit, Programs} = lists:nth(Depth, Population),
	Capacity = length(Programs),
	IsAlreadyIn = lists:member(Program, Programs),
	if not IsAlreadyIn ->
		generate_population(Module, Index - 1,
			lists:keyreplace(Limit, 1, Population, {Limit, [Program | Programs]}),
			1 + (MaxInitDepth - 1)*(Index - 1) div Size, Size);
	true ->
		if Capacity < Limit ->
			generate_population(Module, Index, Population, MaxDepth, Size);
		true ->
			if Depth == MaxDepth ->
				generate_population(Module, Index, Population, MaxDepth + 1, Size);
			true ->
				generate_population(Module, Index, Population, MaxDepth, Size)
			end
		end
	end.
	
%%----------------------------------------------------------------------
%% Function: evaluate_population_single/3
%% Purpose: evaluates population raw fitness using single thread mode
%% Args:   Module as atom()
%% 		   Population as [program()]
%%		   TestCases as [testcase()]
%% Returns: evaluated population as [{RawFitness, program()}]
%% Version: 1.0
%% Remarks: None
%%----------------------------------------------------------------------
evaluate_population_single(Module, Population, TestCases) ->
	lists:map(fun(Program) -> {Module:fitness(Program, TestCases), Program} end, Population).

%%----------------------------------------------------------------------
%% Function: evaluate_population_multi/3
%% Purpose: evaluates population raw fitness using multi-threads mode
%% Args:   Module as atom()
%% 		   Population as [program()]
%% 		   TestCases as [testcase()]
%% Returns: evaluated population as [{RawFitness, program()}]
%% Version: 1.0
%% Remarks: multi-threads version always has better performance when using
%%			multi-core CPU
%%----------------------------------------------------------------------
evaluate_population_multi(Module, Population, TestCases) ->
	Pids = lists:map(fun(_Index) -> 
		spawn(fun evaluate_worker/0) end, lists:seq(1, length(Population))),
	lists:foreach(fun(Index) -> 
		lists:nth(Index, Pids) ! {self(), Module, lists:nth(Index, Population), TestCases} end, 
		lists:seq(1, length(Population))),
	collect_results([], length(Population)).

collect_results(Acc, 0) -> Acc;
collect_results(Acc, Size) ->
	receive {evaluate_result, {Fitness, Program}} ->
		collect_results([{Fitness, Program} | Acc], Size - 1)
	end.

evaluate_worker() ->
	receive {Pid, Module, Program, TestCases} ->
		Pid ! {evaluate_result, {Module:fitness(Program, TestCases), Program}}
	end.

%%----------------------------------------------------------------------
%% Function: evolve_population/2
%% Purpose: evolves evaluated population to produce offsprings
%% Args:   Module as atom()
%% 		   Population as [{Fitness, program()}]
%% Returns: new population as [program()]
%% Version: 1.0
%% Remarks: Current implementation does not use mutation operator
%%----------------------------------------------------------------------
evolve_population(Population) ->
	Settings = erlGP_setting:get_settings(),
	SelectMethod = Settings#setting.selection_method,
	PopulationSize = Settings#setting.population_size,
	evolve_population(Population, [], PopulationSize, SelectMethod).
evolve_population(_Population, OffSprings, 0, _SelectMethod) ->
	OffSprings;
evolve_population(Population, OffSprings, Remaining, SelectMethod) ->
	Prob = random:uniform(),
	Settings = erlGP_setting:get_settings(),
	CrossoverProb = Settings#setting.crossover_prob,
	if Prob < CrossoverProb, Remaining >= 2 ->
		[OffSpring1, OffSpring2] = erlGP_breed:crossover(
			erlGP_select:SelectMethod(Population),
			erlGP_select:SelectMethod(Population)),
		if
			OffSpring1 == {} ->
				evolve_population(Population, [OffSpring2|OffSprings], Remaining - 1, SelectMethod);
			OffSpring2 == {} ->
				evolve_population(Population, [OffSpring1|OffSprings], Remaining - 1, SelectMethod);
			true ->	
				evolve_population(Population, [OffSpring1, OffSpring2|OffSprings], Remaining - 2, SelectMethod)
		end;
	true ->
		OffSpring = erlGP_breed:reproduce(erlGP_select:SelectMethod(Population)),
		evolve_population(Population, [OffSpring|OffSprings], Remaining - 1, SelectMethod)
	end.
	