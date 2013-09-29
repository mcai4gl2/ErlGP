%%%--------------------------------------------------------------------- 
%%% Copyright Geng Li 2008
%%%---------------------------------------------------------------------

%%%--------------------------------------------------------------------- 
%%% Description module erlGP_test
%%%--------------------------------------------------------------------- 
%%% erlGP_test is a generic module which contains functions to run unit
%%%	tests
%%%--------------------------------------------------------------------- 
%%% Exports
%%%--------------------------------------------------------------------- 
%%% test(Function, TestCases)
%%%		tests a function against a list of test cases
%%% run_all_tests()
%%%		runs all tests for ErlGP
%%%---------------------------------------------------------------------
-module(erlGP_test).
%-compile(export_all).
-export([test/2, run_all_tests/0, test/1]).

-include("erlGP_struct.hrl").

%%----------------------------------------------------------------------
%% Function: test/2
%% Purpose: tests a function against a list of test cases
%% Args:   Function is fun()
%%		   TestCases is [testcase()]
%% Returns: {pass, []} if all test cases are passed, 
%%          {failed, [{testcase, actualResult}]} if some tests failed
%% Version: 1.0
%% Remarks: None
%%----------------------------------------------------------------------
test(Function, #testcase{inputs=Inputs, result=ExpectedResult}=TestCase) ->
	AcutalResult = erlang:apply(Function, lists:map(fun(Input) -> Input#input.value end, Inputs)),
	if AcutalResult == ExpectedResult ->
		pass;
	true ->
		{TestCase, AcutalResult}
	end;
test(Function, TestCases) ->
	Outcomes = lists:map(fun(TestCase) -> test(Function, TestCase) end, TestCases),
	Fails = lists:foldl(fun(Outcome, List) -> 
		if is_tuple(Outcome) ->
			[Outcome | List];
		true ->
			List
		end end, [], Outcomes),
	if length(Fails) == 0 -> {pass, []};
	true -> {failed, lists:reverse(Fails)}
	end.

%%----------------------------------------------------------------------
%% Function: test/1
%% Purpose: tests a function against a list of test cases
%% Args:   Module is atom()
%% Returns: {pass, []} if all test cases are passed, 
%%          {failed, [{testcase, actualResult}]} if some tests failed
%% Version: 1.0
%% Remarks: Module_test module must be defined
%%----------------------------------------------------------------------
test(Module) ->
	Name = string:concat(atom_to_list(Module), "_test"),
	TestModule = list_to_atom(Name),
	run_tests(TestModule).

%%----------------------------------------------------------------------
%% Function: run_all_tests/0
%% Purpose: runs all unit tests for ErlGP
%% Args:   None
%% Returns: true after running all tests
%% Version: 1.0
%% Remarks: test results are printed to standard output (console)
%%----------------------------------------------------------------------
run_all_tests() ->
	{ok, Filenames} = file:list_dir("./test"),
	lists:foreach(fun(Filename) ->
		case regexp:match(Filename, ".*_test\\.erl") of
			{match, _, _} ->
				Name = filename:basename(Filename, ".erl"),
				Module = list_to_atom(Name),
				io:format("~w: ~n", [Module]),
				run_tests(Module);
			nomatch ->
				void
			end end,
		Filenames),
	true.

%% runs all functions with test_prefix within a given module through
%% reflection
run_tests(Module) ->
	Functions = Module:module_info(exports),
	lists:foreach(fun({Function, Arity}) ->
		Name = atom_to_list(Function),
		Prefix = string:sub_string(Name, 1, 5),
		Equals = string:equal(Prefix, "test_"),
		if Equals and (Arity == 0) ->
			io:format("~w: ~w~n", [Function, Module:Function()]);
		true ->
			void
		end end,
		Functions).