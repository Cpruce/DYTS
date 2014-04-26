%% CSCI182E - Distributed Systems
%% Harvey Mudd College
%% Distributed fault-tolerant yahtzee tournament system
%% @author Cory Pruce, Colin Stanfill
%% @doc _D157R18U73D_Y4H7533_
-module(yahtzee_manager).

-import(shared).
%% ====================================================================
%%                             Public API
%% ====================================================================
-export(main/1).
%% ====================================================================
%%                             Constants
%% ====================================================================
%% ====================================================================
%%                            Main Function
%% ====================================================================
% The main/1 function.
main(Params)->
	% The first and only parameter is the name the yahtzee_manager 
	% registers itself with. This should be all lowercase with no 
	% @ signs, as on previous assignments, and should also be used 
	% as the node short name to start the network kernel.
	Name = hd(Params),
	% IMPORTANT: Start the empd daemon!
        os:cmd("epmd -daemon"),
        % format microseconds of timestamp to get an
        % effectively-unique node name
        net_kernel:start([list_to_atom(Name), shortnames]),
        register(Name, self()),
	manager_begin(Name).

% Start tournament of a given size upon request from the environment.
manager_begin(Name)->
	receive
		% Data is a tuple {num-players, games-per-match}
		{request_tournament, Pid, {NumPlayers, Gpm}}->
			start_tournament();
		{tournament_info, Pid, Data}->.








