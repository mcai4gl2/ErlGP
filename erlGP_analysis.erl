-module(erlGP_analysis).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").

-include_lib("erlGP_struct.hrl").

do(Q) ->
	F = fun() -> qlc:e(Q) end,
	{atomic, Val} = mnesia:transaction(F),
	Val.

get_all_run_names() ->
	do(qlc:q([X#program_run.name || X <- mnesia:table(run)])).

get_all_data(Name) ->
	do(qlc:q([X || X <- mnesia:table(Name)])).

remove_data(Name) ->
	Oid = {run, Name},
	F = fun() -> mnesia:delete(Oid) end,
	mnesia:transaction(F),
	mnesia:delete_table(Name).

start_analysis() ->
	mnesia:start().

finish_analysis() ->
	mnesia:stop().
	
get_execution_time(Name) ->
	Times = do(qlc:q([X#data.time || X <- mnesia:table(Name)])),
	lists:foldl(fun({Second, _Milisecond}, Sum) -> Second + Sum end, 0, Times).

get_times(Name) ->
	Times = do(qlc:q([X#data.time || X <- mnesia:table(Name)])),
	lists:map(fun({Second, _Milisecond}) -> Second end, Times).
	
get_peek_memory_cost(Name) ->
	Costs = do(qlc:q([X#data.memory || X <- mnesia:table(Name)])),
	lists:max(Costs) / 1000000.

get_num_of_gens(Name) ->
	Gens = do(qlc:q([X#data.gen_num || X <- mnesia:table(Name)])),
	length(Gens).

get_fitness(Name, Gen) ->
	Population = hd(do(qlc:q([X#data.population || X <- mnesia:table(Name), 
		X#data.gen_num == Gen]))),
	lists:map(fun({Val, _Program}) -> Val end, Population).

get_fitness_statistics(Name, Gen) ->
	Fitness = get_fitness(Name, Gen),
	lists:sum(Fitness) / length(Fitness).

get_depth(Name, Gen) ->
	Population = hd(do(qlc:q([X#data.population || X <- mnesia:table(Name), 
		X#data.gen_num == Gen]))),
	Programs = lists:map(fun({_Val, Program}) -> Program end, Population),
	lists:map(fun erlGP_program:depth/1, Programs).

get_depth_statistics(Name, Gen) ->
	Depth = get_depth(Name, Gen),
	lists:sum(Depth) / length(Depth).
	
get_programs(Name, Gen) ->
	Population = hd(do(qlc:q([X#data.population || X <- mnesia:table(Name), 
		X#data.gen_num == Gen]))),
	lists:map(fun({_Val, Program}) -> erlGP_program:output_program(Program) end, Population).
	
get_population(Name, Gen) ->
	hd(do(qlc:q([X#data.population || X <- mnesia:table(Name), 
		X#data.gen_num == Gen]))).