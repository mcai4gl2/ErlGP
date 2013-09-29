%%%--------------------------------------------------------------------- 
%%% Copyright Geng Li 2008
%%%---------------------------------------------------------------------

%%%--------------------------------------------------------------------- 
%%% Description module multiplexer11_test
%%%--------------------------------------------------------------------- 
%%% multiplexer11_test module contains unit tests for multiplexer11 module
%%%--------------------------------------------------------------------- 
%%% Exports
%%%--------------------------------------------------------------------- 
%%% all unit tests
%%%---------------------------------------------------------------------
-module(multiplexer11_test).
-compile(export_all).

-include("../erlGP_struct.hrl").

%% tests the length of functions list, should be 4
test_functions_length() ->
	Functions = multiplexer11:functions(),
	erlGP_test:test(fun length/1, 
		[#testcase{inputs=[#input{name=functions, value=Functions}], result=4}]).

%% tests the length of terminals list, should be 11
test_terminals_length() ->
	Terminals = multiplexer11:terminals(),
	erlGP_test:test(fun length/1, 
		[#testcase{inputs=[#input{name=terminals, value=Terminals}], result=11}]).