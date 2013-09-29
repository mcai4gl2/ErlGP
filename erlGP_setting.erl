%%%--------------------------------------------------------------------- 
%%% Copyright Geng Li 2008
%%%---------------------------------------------------------------------

%%%--------------------------------------------------------------------- 
%%% Description module erlGP_setting
%%%--------------------------------------------------------------------- 
%%% erlGP_setting is used to access various settings about genetic
%%% programming
%%%--------------------------------------------------------------------- 
%%% Exports
%%%--------------------------------------------------------------------- 
%%% get_settings()
%%%		returns genetic programming settings
%%%---------------------------------------------------------------------
-module(erlGP_setting).
-compile(export_all).

-include("erlGP_struct.hrl").

%%----------------------------------------------------------------------
%% Function: get_settings/0
%% Purpose: returns genetic programming settings
%% Args:   None
%% Returns: setting()
%% Version: 1.0
%% Remarks: None
%%----------------------------------------------------------------------
get_settings() ->
	#setting{
	}.