%%%--------------------------------------------------------------------- 
%%% Copyright Geng Li 2008
%%%---------------------------------------------------------------------

%%%--------------------------------------------------------------------- 
%%% Description module erlGP_utility_test
%%%--------------------------------------------------------------------- 
%%% erlGP_utility_test module contains unit tests for erlGP_utility module
%%%--------------------------------------------------------------------- 
%%% Exports
%%%--------------------------------------------------------------------- 
%%% all unit tests
%%%---------------------------------------------------------------------
-module(erlGP_utility_test).
-compile(export_all).

-include("erlGP_struct.hrl").

%% tests num_of_possible_programs function
test_num_of_possible_programs() ->
	TestCases = [
		#testcase{
		inputs=[
			#input{name=module, value=multiplexer11},
			#input{name=depth, value=1}
			], 
		result=11
		},
		#testcase{
		inputs=[
			#input{name=module, value=multiplexer11},
			#input{name=depth, value=2}
			], 
		result=99
		}
		],
	erlGP_test:test(fun erlGP_utility:num_of_possible_programs/2, TestCases).