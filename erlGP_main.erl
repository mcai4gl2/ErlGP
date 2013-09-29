%%%--------------------------------------------------------------------- 
%%% Copyright Geng Li 2008
%%%--------------------------------------------------------------------- 

%%%--------------------------------------------------------------------- 
%%% Description module erlGP_main
%%%--------------------------------------------------------------------- 
%%% erlGP_main module contains main program entrance for erlGP
%%%--------------------------------------------------------------------- 
%%% Exports
%%%--------------------------------------------------------------------- 
%%% main/1, 2
%%%		performance genetic programming on specified domain
%%%---------------------------------------------------------------------
-module(erlGP_main).
-compile(export_all).

-include("erlGP_struct.hrl").

main(Module) ->
	Settings = erlGP_setting:get_settings(),
	main(Module, Settings#setting.max_generation).
main(Module, MaxGeneration) ->
	io:format("Module: ~w~n", [Module]),
	io:format("Generating Generation 0 ... ~n"),
	Population = erlGP_core:generate_population(Module),
	TestCases = Module:generate_test_cases(),
	DumpFile = erlGP_log:get_time_stamp_as_atom(),
	erlGP_mnesia:startup(),
	erlGP_mnesia:create_new_data(DumpFile, Module, DumpFile, MaxGeneration, "Tournament_select"),
	io:format("~n"),
	main(Module, 0, MaxGeneration, Population, TestCases, DumpFile).
main(Module, CurrentGen, MaxGen, Population, TestCases, DumpFile) ->
	if CurrentGen =< MaxGen ->
		io:format("Evaluating Generation ~w ... ~n", [CurrentGen]),
		{_,StartSecond,StartMicroSec} = erlang:now(),
		EvaluatedPopulation = erlGP_core:evaluate_population_multi(Module, Population, TestCases),
		{_,FinishSecond, FinishMicroSec} = erlang:now(),
		io:format("Time used: ~w s,~w ms~n", [FinishSecond - StartSecond, FinishMicroSec - StartMicroSec]),
		{Fit, Program} = lists:nth(length(EvaluatedPopulation), lists:keysort(1, EvaluatedPopulation)),
		io:format("Best program: ~w~n", [erlGP_program:output_program(Program)]),
		io:format("Best Raw fitness: ~w~n", [Fit]),
		io:format("Best program depth: ~w~n", [erlGP_program:depth(Program)]),
		io:format("Evolving for next generation ... ~n"),
		{total, Memory} = hd(erlang:memory()),
		io:format("Current Memory Usage: ~w~n", [Memory]),
		io:format("Start dumping ... ", []),
		erlGP_mnesia:add_data(DumpFile, CurrentGen, EvaluatedPopulation, 
			{FinishSecond - StartSecond, FinishMicroSec - StartMicroSec},
			Memory),
		io:format("finish~n", []),
		ReadyPopulation = Module:prepare_population(EvaluatedPopulation),
		NewPopulation = erlGP_core:evolve_population(ReadyPopulation),
		io:format("~n"),
		main(Module, CurrentGen + 1, MaxGen, NewPopulation, TestCases, DumpFile);
	true -> 
		{Tag, Val} = erlGP_mnesia:create_backup(),
		if Tag == ok ->
			io:format("Backup ~w created~n", [Val]);
		true ->
			io:format("Do not need to create backup now, backup in ~w times~n", [Val])
		end,
		erlGP_mnesia:shutdown(),
		io:format("Program finishes ~n")
	end.