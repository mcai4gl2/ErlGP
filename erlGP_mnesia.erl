%%%--------------------------------------------------------------------- 
%%% Copyright Geng Li 2008
%%%---------------------------------------------------------------------

%%%--------------------------------------------------------------------- 
%%% Description module erlGP_mnesia
%%%--------------------------------------------------------------------- 
%%% erlGP_mnesia module contains all functions related to storing
%%% information into mnesia database
%%%--------------------------------------------------------------------- 
%%% Exports
%%%--------------------------------------------------------------------- 
%%% 
%%%---------------------------------------------------------------------
-module(erlGP_mnesia).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").

-include("erlGP_struct.hrl").

create_table() ->
	mnesia:start(),
	mnesia:create_table(run, 
		[
			{attributes, record_info(fields, program_run)},
			{disc_only_copies, [node()]}
		]),
	mnesia:stop().

startup() ->
	mnesia:start(),
	mnesia:wait_for_tables([run], 20000).

shutdown() -> mnesia:stop().

reset_tables() ->
	lists:foreach(fun mnesia:delete_table/1, erlGP_analysis:get_all_run_names()),
	mnesia:clear_table(program_run),
	ok.
	
create_new_data(Name, Domain, Time, Num_of_gens, Note) ->
	F = fun() -> mnesia:write({run, Name, Domain, Time, Num_of_gens, Note}) end,
	mnesia:transaction(F),
	mnesia:create_table(Name, 
		[
			{attributes, record_info(fields, data)},
			{disc_only_copies, [node()]}
		]),
	{ok, Name}.
	
add_data(Name, Gen_num, Population, Time, Memory) ->
	F = fun() -> mnesia:write({Name, Gen_num, Population, Time, Memory}) end,
	mnesia:transaction(F),
	ok.

create_backup() ->
	Tables = erlGP_analysis:get_all_run_names(),
	#setting{mnesia_backup_dir = Dir, mnesia_backup_frequency = Frequency} = 
		erlGP_setting:get_settings(),
	Result = length(Tables) rem Frequency,
	if Result == 0 ->
		TimeStamp = erlGP_log:get_time_stamp_as_atom(),
		Name = list_to_atom(lists:concat([Dir, TimeStamp, '_', length(Tables), '.bak'])),
		mnesia:backup(Name),
		{ok, Name};
	true ->
		{noneed, Frequency - Result}
	end.