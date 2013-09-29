%%%--------------------------------------------------------------------- 
%%% Copyright Geng Li 2008
%%%--------------------------------------------------------------------- 

%%%--------------------------------------------------------------------- 
%%% Description module erlGP_select
%%%--------------------------------------------------------------------- 
%%% erlGP_select module contains all functions related to selection
%%%--------------------------------------------------------------------- 
%%% Exports
%%%--------------------------------------------------------------------- 
%%% fitness_proportionate_select/1
%%%		selects an individual from population based on fittness
%%%		proportionate selection
%%%---------------------------------------------------------------------
-module(erlGP_select).
-compile(export_all).

-include("erlGP_struct.hrl").

%%----------------------------------------------------------------------
%% Function: fitness_proportionate_select/1
%% Purpose: selects an individual from population based on fittness
%% 			proportionate selection
%% Args:   SortedPopulation as [{NormalizedFitness, program()}]
%% Returns: selected program
%% Version: 1.0
%% Remarks: the sortedPopulation needs to be sorted basd on normalized-
%%			fitness using asc-order
%%----------------------------------------------------------------------
fitness_proportionate_select(SortedPopulation) ->
	Sum = lists:sum(lists:map(fun({Val, _}) -> Val end, SortedPopulation)),
	Pos = random:uniform() * Sum,
	Index = fitness_proportionate_select(SortedPopulation, 0, Pos, 1),
	{_, Program} = lists:nth(Index, SortedPopulation),
	Program.
fitness_proportionate_select([Current | Tail], Sum, Pos, Index) ->
	{Value, _} = Current,
	if Value + Sum >= Pos -> Index;
	true -> fitness_proportionate_select(Tail, Sum + Value, Pos, Index + 1)
	end;
fitness_proportionate_select([], _, _, _) -> -1.

tournament_select(Population) ->
	Settings = erlGP_setting:get_settings(),
	Size = Settings#setting.tournament_size,
	Tournament = create_tournament(Population, [], 0, Size),
	SortedTournament = lists:keysort(1, Tournament),
	{Value, _} = lists:last(SortedTournament),
	FinalTournament = remove_tournament(Tournament, Value),
	{_, Program} = erlGP_utility:random_choice(FinalTournament),
	Program.
	
create_tournament(Population, Tournament, Current, Size) ->
	if Current == Size -> Tournament;
	true ->
		create_tournament(Population, 
			[erlGP_utility:random_choice(Population) | Tournament],
			Current + 1,
			Size)
	end.

remove_tournament(Tournament, Val) ->
	{Val1, _} = hd(Tournament),
	if Val == Val1 ->
		Tournament;
	true ->
		[_ | Tail] = Tournament,
		Tail
	end.