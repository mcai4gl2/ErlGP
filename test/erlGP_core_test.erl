%%%--------------------------------------------------------------------- 
%%% Copyright Geng Li 2008
%%%---------------------------------------------------------------------

%%%--------------------------------------------------------------------- 
%%% Description module erlGP_core_test
%%%--------------------------------------------------------------------- 
%%% erlGP_core_test module contains unit tests for erlGP_core module
%%%--------------------------------------------------------------------- 
%%% Exports
%%%--------------------------------------------------------------------- 
%%% all unit tests
%%%---------------------------------------------------------------------
-module(erlGP_core_test).
-compile(export_all).

-include("erlGP_struct.hrl").

%% tests evaluate/2 function based on several test cases
test_evaluate() ->
	{value, NotDef} = lists:keysearch('not', #function_def.name, multiplexer11:functions()),
	TestCases = [
		#testcase{
		inputs=[#input{name=variable, value=[#input{name='A0', value=true}]},
			    #input{name=program, value=#program{name='not', 
					func=NotDef#function_def.func, args=['A0']}}
			   ], 
		result=false
		},
		#testcase{
		inputs=[#input{name=variable, value=[#input{name='A0', value=true}]},
			    #input{name=program, value='A0'}
			   ], 
		result=true
		}
		],
	erlGP_test:test(fun erlGP_core:evaluate/2, TestCases).

%% tests whether generate_population function produces non-repeated list of programs
test_generate_population() ->
	Population = erlGP_core:generate_population(multiplexer11),
	{Result, Reason} = check_duplicate(Population),
	if Result == true ->
		{pass, []};
	true ->
		{fail, [Reason, Population]}
	end.

%% auxiliary function which checks if there is duplication in a list or not
check_duplicate(List) ->
	[Head | Tail] = List,
	Duplicated = lists:member(Head, Tail),
	if Duplicated ->
		{false, Head};
	true ->
		if Tail == [] ->
			{true, void};
		true ->
			check_duplicate(Tail)
		end
	end.