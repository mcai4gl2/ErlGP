%%%--------------------------------------------------------------------- 
%%% Copyright Geng Li 2008
%%%---------------------------------------------------------------------

%%%--------------------------------------------------------------------- 
%%% Description module erlGP_breed_test
%%%--------------------------------------------------------------------- 
%%% erlGP_breed_test module contains unit tests for erlGP_breed module
%%%--------------------------------------------------------------------- 
%%% Exports
%%%--------------------------------------------------------------------- 
%%% all unit tests
%%%---------------------------------------------------------------------
-module(erlGP_breed_test).
-compile(export_all).

-include("erlGP_struct.hrl").

%% tests crossover/3 function based on several test cases
test_crossover() ->
	TestCases = [
		#testcase{
		inputs=[
			#input{name=program, value=#program{name='and', func=func, args=['A0', 'A1']}},
			#input{name=replacement, value='B0'},
			#input{name=position, value=1}
			], 
		result= 'B0'
		},
		#testcase{
		inputs=[
			#input{name=program, value=#program{name='and', func=func, args=['A0', 'A1']}},
			#input{name=replacement, value='B0'},
			#input{name=position, value=2}
			], 
		result= #program{name='and', func=func, args=['B0', 'A1']}
		},
		#testcase{
		inputs=[
			#input{name=program, value=#program{name='and', func=func, args=['A0', 'A1']}},
			#input{name=replacement, value='B0'},
			#input{name=position, value=3}
			], 
		result= #program{name='and', func=func, args=['A0', 'B0']}
		},
		#testcase{
		inputs=[
			#input{name=program, value=#program{name='and', func=func, args=['A0', 'A1']}},
			#input{name=replacement, value=#program{name='and', func=func, args=['A0', 'A1']}},
			#input{name=position, value=3}
			], 
		result= #program{name='and', func=func, args=['A0', 
			#program{name='and', func=func, args=['A0', 'A1']}]}
		},
		#testcase{
		inputs=[
			#input{name=program, value='A0'},
			#input{name=replacement, value=#program{name='and', func=func, args=['A0', 'A1']}},
			#input{name=position, value=1}
			], 
		result= #program{name='and', func=func, args=['A0', 'A1']}
		}
		],
	erlGP_test:test(fun erlGP_breed:crossover/3, TestCases).