%%%--------------------------------------------------------------------- 
%%% Copyright Geng Li 2008
%%%---------------------------------------------------------------------

%%%--------------------------------------------------------------------- 
%%% Description record definitions erlGP_struct
%%%--------------------------------------------------------------------- 
%%% ErlGP project uses a lot of data structures. All data structures are
%%% defined using record rather than plaint tuple.
%%%--------------------------------------------------------------------- 
%%% Exports
%%%--------------------------------------------------------------------- 
%%% All records definitions
%%%---------------------------------------------------------------------

%%---------------------------------------------------------------------
%% Data Type: function_def
%% where:
%%    name: A string (default is undefined)
%%    arity: An integer (default is undefined)
%%    fun: A fun definition (default is undefined)
%% Remarks: function_def defines a function
%%----------------------------------------------------------------------
-record(function_def, {name, arity, func}).

%%---------------------------------------------------------------------
%% Data Type: program
%% where:
%%    name: A string (default is undefined)
%%    fun: A fun definition (default is undefined)
%%    args: A list of arguments, elem can be either atom() or program
%%			(default is [] i.e. empty argument list)
%% Remarks: program is recursively defined. fun is not actually needed.
%%			But with the fun stored here, there is no need to search the
%%			function definition in run-time. This is persumed to increase
%%			performance. The tradeoff is memory. But from results from
%%			previous version, the memory is not a problem
%%----------------------------------------------------------------------
-record(program, {name, func, args=[]}).

%%---------------------------------------------------------------------
%% Data Type: input
%% where:
%%    name: A string (default is undefined)
%%    value: A variable of any type term() (default is undefined)
%% Remarks: Input is used to store a key-value pair which will be used by
%%			testcase
%%----------------------------------------------------------------------
-record(input, {name, value}).

%%---------------------------------------------------------------------
%% Data Type: testcase
%% where:
%%    inputs: A list of input [input()] (default is empty list [])
%%	  result: A Vairable of any type term() (default is undefined)
%% Remarks: testcase stores a list of inputs and the expected result
%%----------------------------------------------------------------------
-record(testcase, {inputs = [], result}).

%%---------------------------------------------------------------------
%% Data Type: setting
%% where:
%% 	   max_init_depth: A int() stores the max depth of generatation 0 programs
%%					   default is 6 according to Koza
%% 	   population_size: A int() stores the population size, default is 4000
%%					    according to Koza
%%     max_crossover_depth: A int() stores the max depth of a offspring created
%%							using crossover, default is 17 according to Koza
%%	   crossover_prob: A int() stores the probaility of breading through crossover
%%					   default is 0.9 according to Koza
%% 	   selection_method: A atom() stores the selection method used
%% 	   max_generation: max number of generations erlGP runs
%% Remarks: setting stores all environment settings for erlGP.
%%			max_crossover_depth:
%%			Checking of this parameter is built in to crossover function defined
%%			in erlGP_breed. If do not want to limit the crossover offspring's depth,
%%			set this number to very big number
%%			crossover_prob:
%%			Currently, mutation is not used, only crossover and reproduction are
%%			used. So the probability for reproduction is: 1 - crossover_prob
%%----------------------------------------------------------------------
-record(setting, {
	max_init_depth = 6, 
	population_size = 4000,
	max_crossover_depth = 17,
	crossover_prob = 0.9,
	selection_method = tournament_select,
	max_generation = 50,
	mnesia_backup_dir = "./backup/",
	mnesia_backup_frequency = 10,
	tournament_size = 7
	}).

-record(program_run, {name, domain, time, num_of_gens, note}).

-record(data, {gen_num, population, time, memory}).