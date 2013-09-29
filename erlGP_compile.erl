%%%--------------------------------------------------------------------- 
%%% Copyright Geng Li 2008
%%%--------------------------------------------------------------------- 

%%%--------------------------------------------------------------------- 
%%% Description module erlGP_compile
%%%--------------------------------------------------------------------- 
%%% ErlGP project has a number of different modules. erlGP_compile module
%%% provides a easy way to compile all files in Erlang emulator. By using
%%% this module, there is no need to type in commands to recompile all
%%%	files. Command reset(). will reset the emulator environment 
%%%--------------------------------------------------------------------- 
%%% Exports
%%%--------------------------------------------------------------------- 
%%% compile()
%%%		compiles all source codes in ErlGP project
%%% compile(File)
%%%		compiles a file with given filename
%%%--------------------------------------------------------------------- 
-module(erlGP_compile).
-export([compile/0, compile/1]).

%%----------------------------------------------------------------------
%% Function: compile/0
%% Purpose: compiles all source codes in erlGP
%% Args:   None
%% Returns: true after compiling all codes
%% Version: 1.0
%% Remarks: File list needs to be maintained manually. There should be
%%			very little change after all modules are introduced
%%----------------------------------------------------------------------
compile() ->
	Files = ["domain/multiplexer11.erl", "test/multiplexer11_test.erl", 
			 "erlGP_test.erl", 
			 "erlGP_utility", "test/erlGP_utility_test.erl", 
			 "erlGP_program.erl", "test/erlGP_program_test.erl",
			 "erlGP_core.erl", "test/erlGP_core_test.erl",
			 "erlGP_setting.erl",
			 "erlGP_breed.erl", "test/erlGP_breed_test.erl",
			 "erlGP_select.erl",
			 "erlGP_log.erl",
			 "erlGP_main.erl",
			 "erlGP_mnesia.erl",
			 "erlGP_analysis.erl"
			],
	lists:map(fun compile/1, Files), 
	true.

%%----------------------------------------------------------------------
%% Function: compile/1
%% Purpose: compiles an erlang source file
%% Args:   File is string(), whhich stores the file name
%% Returns: void
%% Version: 1.0
%% Remarks: None
%%----------------------------------------------------------------------
compile(File) ->
	io:format("~w~n", [c:c(File)]),
	void.