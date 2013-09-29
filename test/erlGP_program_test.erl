%%%--------------------------------------------------------------------- 
%%% Copyright Geng Li 2008
%%%---------------------------------------------------------------------

%%%--------------------------------------------------------------------- 
%%% Description module erlGP_program_test
%%%--------------------------------------------------------------------- 
%%% multiplexer11_test module contains unit tests for erlGP_program module
%%%--------------------------------------------------------------------- 
%%% Exports
%%%--------------------------------------------------------------------- 
%%% all unit tests
%%%---------------------------------------------------------------------
-module(erlGP_program_test).
-compile(export_all).

-include("../erlGP_struct.hrl").

%% tests the depth function, now only 3 test cases are used, further tests can be done
%% after developing generate_program function
test_depth() ->
	TestCases = [
		#testcase{
		inputs=[#input{name=program, value=#program{name='not', func=func, args=['A0']}}], 
		result=2
		},
		#testcase{
		inputs=[#input{name=program, value=#program{name='and', func=func, args=['A0', 'A1']}}],
		result=2
		},
		#testcase{
		inputs=[#input{name=program, value=#program{name='and', func=func, args=[
			#program{name='or', func=func, args=['A2', 'A3']}, 'D1']}}],
		result=3
		}
		],
	erlGP_test:test(fun erlGP_program:depth/1, TestCases).

%% tests generate_full function. This test function also uses erlGP_program:depth, so an error
%% here may not be bugs in generate_full
test_generate_full() ->
	TestCases = lists:map(fun(Depth) -> #testcase{
		inputs=[#input{name=program, value=erlGP_program:generate_full(multiplexer11, Depth)}],
		result=Depth
		} end, lists:seq(1, 17)),
	erlGP_test:test(fun erlGP_program:depth/1, TestCases).

%% tests num_of_nodes function based on several inputs
test_num_of_nodes() ->
	TestCases = [
		#testcase{
		inputs=[#input{name=program, value=#program{name='not', func=func, args=['A0']}}], 
		result=2
		},
		#testcase{
		inputs=[#input{name=program, value=#program{name='and', func=func, args=['A0', 'A1']}}],
		result=3
		},
		#testcase{
		inputs=[#input{name=program, value=#program{name='and', func=func, args=[
			#program{name='or', func=func, args=['A2', 'A3']}, 'D1']}}],
		result=5
		}
		],
	erlGP_test:test(fun erlGP_program:num_of_nodes/1, TestCases).

%% tests get_node_at function based on several inputs
test_get_node_at() ->
	TestCases = [
		#testcase{
		inputs=[#input{name=program, value=#program{name='and', func=func, args=[
			#program{name='or', func=func, args=['A2', 'A3']}, 'D1']}},
			#input{name=index, value=1}],
		result = #program{name='and', func=func, args=[
			#program{name='or', func=func, args=['A2', 'A3']}, 'D1']}
		},
		#testcase{
		inputs=[#input{name=program, value=#program{name='and', func=func, args=[
			#program{name='or', func=func, args=['A2', 'A3']}, 'D1']}},
			#input{name=index, value=2}],
		result = #program{name='or', func=func, args=['A2', 'A3']}
		},
		#testcase{
		inputs=[#input{name=program, value=#program{name='and', func=func, args=[
			#program{name='or', func=func, args=['A2', 'A3']}, 'D1']}},
			#input{name=index, value=3}],
		result = 'A2'
		},
		#testcase{
		inputs=[#input{name=program, value=#program{name='and', func=func, args=[
			#program{name='or', func=func, args=['A2', 'A3']}, 'D1']}},
			#input{name=index, value=4}],
		result = 'A3'
		},
		#testcase{
		inputs=[#input{name=program, value=#program{name='and', func=func, args=[
			#program{name='or', func=func, args=['A2', 'A3']}, 'D1']}},
			#input{name=index, value=5}],
		result = 'D1'
		}
		],
	erlGP_test:test(fun erlGP_program:get_node_at/2, TestCases).
		
%% tests output_program function based on some inputs
test_output_program() ->
	TestCases = [
		#testcase{
		inputs=[#input{name=program, value=#program{name='not', func=func, args=['A0']}}], 
		result={'not', ['A0']}
		},
		#testcase{
		inputs=[#input{name=program, value=#program{name='and', func=func, args=['A0', 'A1']}}],
		result={'and', ['A0', 'A1']}
		},
		#testcase{
		inputs=[#input{name=program, value=#program{name='and', func=func, args=[
			#program{name='or', func=func, args=['A2', 'A3']}, 'D1']}}],
		result={'and', [{'or', ['A2', 'A3']}, 'D1']}
		}
		],
	erlGP_test:test(fun erlGP_program:output_program/1, TestCases).