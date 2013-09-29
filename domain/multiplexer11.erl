%%%--------------------------------------------------------------------- 
%%% Copyright Geng Li 2008
%%%---------------------------------------------------------------------

%%%--------------------------------------------------------------------- 
%%% Description module multiplexer11
%%%--------------------------------------------------------------------- 
%%% multiplexer11 defines a problem domain used by erlGP. For a typical
%%% problem domain, the following functions should be defined: 
%%% 	functions/0 - returns a list of function definitions
%%%		terminals/0 - returns a list of terminals denoted using atom
%%%		fitness/1   - calculate raw fitness of a program
%%%     generate_test_cases/0 - generates test cases for evaluation
%%%		prepare_population/1 - prepare population for breeding
%%%--------------------------------------------------------------------- 
%%% Exports
%%%--------------------------------------------------------------------- 
%%% functions()
%%% 	returns a list of function definitions. In multiplexer11, avaliable
%%%		functions are 'and', 'or', 'if' and 'not'
%%% terminals()
%%%		returns a list of terminals
%%%	fitness(Program)
%%%		compute raw fitness of a program based on 2048 test cases
%%% fitness(Program, TestCases)
%%%		same as fitness/1, but test cases are not generated within function,
%%%		this will improve performance a lot
%%% generate_test_cases()
%%%		generates all 2048 test cases
%%% adjusted_fitness(Fitness)
%%%		converts raw fitness to adjusted fitness 
%%%	prepare_population/1
%%% 	prepare population for breeding
%%%---------------------------------------------------------------------
-module(multiplexer11).
-compile(export_all).
-export([functions/0, terminals/0, fitness/1, fitness/2]).
-export([generate_test_cases/0, adjusted_fitness/1, prepare_population/1]).

-include("erlGP_struct.hrl").

%%----------------------------------------------------------------------
%% Function: functions/0
%% Purpose: returns a list of function definitions
%% Args:   None
%% Returns: [function_def()]
%% Version: 1.0
%% Remarks: None
%%----------------------------------------------------------------------
functions() ->
	[#function_def{name='and', arity=2,
		func=fun(V, [X, Y]) -> erlGP_core:evaluate(V, X) andalso 
			erlGP_core:evaluate(V, Y) end},
	 #function_def{name='or', arity=2,
		func=fun(V, [X, Y]) -> erlGP_core:evaluate(V, X) orelse 
			erlGP_core:evaluate(V, Y) end},
	 #function_def{name='if', arity=3,
		func=fun(V, [X, Y, Z]) ->
			Value = erlGP_core:evaluate(V, X),
			if Value -> erlGP_core:evaluate(V, Y);
			true -> erlGP_core:evaluate(V, Z) 
			end 
		end},
	 #function_def{name='not', arity=1,
		func=fun(V, [X]) -> not erlGP_core:evaluate(V, X) end}		
	].

%%----------------------------------------------------------------------
%% Function: terminals/0
%% Purpose: returns a list of terminals
%% Args:   None
%% Returns: [atom()]
%% Version: 1.0
%% Remarks: None
%%----------------------------------------------------------------------
terminals() ->
	['D0', 'D1', 'D2', 'D3', 'D4', 'D5', 'D6', 'D7', 'A0', 'A1', 'A2'].

%%----------------------------------------------------------------------
%% Function: generate_test_cases/0
%% Purpose: generates 2048 test cases for multiplexer 11 problem
%% Args:   None
%% Returns: [testcase()]
%% Version: 1.0
%% Remarks: None
%%----------------------------------------------------------------------
generate_test_cases() ->
	[convert_to_input(generate_test_case(X, 8, Y, 3)) || X <- lists:seq(0, 255), Y <- lists:seq(0, 7)].

%% generates a test case given the integer value for data bits and address bits, 
%% auxiliary function used by generate_test_cases
generate_test_case(DataValue, DataLength, AddressValue, AddressLength) ->
	Data = convert_to_binary(DataValue, DataLength),
	Address = convert_to_binary(AddressValue, AddressLength),
	Result = lists:nth(DataLength - AddressValue, Data),
	{lists:append(Data, Address), Result}.

%% converts integer value to binary represented by list, 
%% auxiliary function used by generate_test_case
convert_to_binary(Value, Length) ->
	Result = lists:map(fun(V) -> V - 48 end, hd(io_lib:format("~.2B",[Value]))),
	lists:append(lists:duplicate(Length - length(Result), 0), Result).

%% converts 0, 1 to false, true and adds terminal names, 
%% auxiliary function used by generate_test_case
convert_to_input({Input, Result}) ->
	Convert = fun(E) -> if E == 0 -> false; true -> true end end,
	InputC = lists:map(Convert, Input),
	Terminals = terminals(),
	#testcase{inputs=[#input{name=lists:nth(X, Terminals), value=lists:nth(X, InputC)} || X <- lists:seq(1, length(InputC))], 
		result=Convert(Result)}.

%%----------------------------------------------------------------------
%% Function: fitness/1
%% Purpose: calculate the raw fitness of a program
%% Args:   Program is a program()
%% Returns: int()
%% Version: 1.0
%% Remarks: fitness/1 generates test cases every time the function is
%%			called. This will affect performance. See fitness/2
%%----------------------------------------------------------------------
fitness(Program) ->
	fitness(Program, generate_test_cases(), 0).

%%----------------------------------------------------------------------
%% Function: fitness/2
%% Purpose: calculate the raw fitness of a program
%% Args:   Program is a program()
%%		   TestCases is [testcase()]
%% Returns: int()
%% Version: 1.0
%% Remarks: fitness/2 calculates raw fitness basded on input test cases. 
%%			So test cases can be generated only once and reused to improve
%%			performance
%%----------------------------------------------------------------------
fitness(Program, TestCases) ->
	fitness(Program, TestCases, 0).
	
%% calculate raw fitness recursively, auxiliary function used by fitness/2
fitness(_, [], Hits) ->
	Hits;
fitness(Program, [#testcase{inputs=Input, result=Result} | Rest], Hits) ->
	Res = erlGP_core:evaluate(Input, Program),
	if Res == Result ->
		fitness(Program, Rest, Hits + 1);
	true ->
		fitness(Program, Rest, Hits)
	end.

%%----------------------------------------------------------------------
%% Function: adjusted_fitness/1
%% Purpose: convert raw fitness to standard fitness
%% Args:   Fitness is int()
%% Returns: decimal value
%% Version: 1.0
%% Remarks: None
%%----------------------------------------------------------------------
adjusted_fitness(Fitness) ->
	1/(1 + (2048 - Fitness)).
	
%%----------------------------------------------------------------------
%% Function: prepare_population/1
%% Purpose: converts population with raw fitness to population suitable
%%			for selection
%% Args:   Population is [{RawFitness, program()}]
%% Returns: [{AdjustedFitenss, program()}]
%% Version: 1.0
%% Remarks: None
%%----------------------------------------------------------------------
prepare_population(Population) ->
	AdjustedPopulation = lists:map(fun({Fitness, Program}) -> {adjusted_fitness(Fitness), Program} end, Population),
	Sum = lists:sum(lists:map(fun({Fitness, _}) -> Fitness end, AdjustedPopulation)),
	NormalisedPopulation =
		lists:map(fun({Fitness, Program}) -> {Fitness/Sum, Program} end, AdjustedPopulation),
	lists:keysort(1, NormalisedPopulation).
