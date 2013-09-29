%%%--------------------------------------------------------------------- 
%%% Copyright Geng Li 2008
%%%--------------------------------------------------------------------- 

%%%--------------------------------------------------------------------- 
%%% Description module erlGP_log
%%%--------------------------------------------------------------------- 
%%% erlGP_log module contains all functions related to logging
%%%--------------------------------------------------------------------- 
%%% Exports
%%%--------------------------------------------------------------------- 
%%% get_filename/0
%%%		returns the filename of log file
%%% dump_information/3
%%%		dumps generation information into log file
%%% dump_close/1
%%%		dumps close information 
%%%---------------------------------------------------------------------
-module(erlGP_log).
-compile(export_all).

%% auxiliary function used by get_filename
get_time_stamp() ->
	{Year, Month, Date} = erlang:date(),
	{Hour, Miniute, _} = erlang:time(),
	lists:concat([Year, '_', Month, '_', Date, '_', Hour, '_', Miniute]).

get_time_stamp_as_atom() ->
	list_to_atom(get_time_stamp()).

%%----------------------------------------------------------------------
%% Function: get_filename/0
%% Purpose: returns the filename of log file
%% Args:   None
%% Returns: setting()
%% Version: 1.0
%% Remarks: None
%%----------------------------------------------------------------------
get_filename() ->
	Name = list_to_atom(lists:concat(['.', '/', 'log', '/', get_time_stamp(), '.', 'txt'])),
	{ok, File} = file:open(Name, write),
	File.

%%----------------------------------------------------------------------
%% Function: dump_information/3
%% Purpose: dumps generation information into log file
%% Args:   Filename as string()
%%		   Population as [program()]
%%         Generation as int()
%% Returns: true
%% Version: 1.0
%% Remarks: None
%%----------------------------------------------------------------------
dump_information(File, Population, Generation) ->
	io:format(File, "~nDumping population statistics for generation ~w:~n", [Generation]),
	io:format(File, "Population Size: ~w~n", [length(Population)]),
	io:format(File, "Population Raw Fitness: ~n", []),
	lists:map(fun({Fitness, _}) -> io:format(File, "~w ", [Fitness]) end, Population),
	io:format(File, "~n", []),
	io:format(File, "Population Depth: ~n", []),
	lists:map(fun({_, Program}) -> io:format(File, "~w ", [erlGP_program:depth(Program)]) end, Population),
	io:format(File, "~n~n", []).

%%----------------------------------------------------------------------
%% Function: dump_close/1
%% Purpose: dumps close information
%% Args:   Filename as string()
%% Returns: true
%% Version: 1.0
%% Remarks: None
%%----------------------------------------------------------------------
dump_close(File) ->
	io:format(File, "Finish Time: ~w~n", [erlang:localtime()]),
	file:close(File),
	io:format("Dump file closed~n",[]).