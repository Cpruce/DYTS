%% CSCI182E - Distributed Systems
%% Harvey Mudd College
%% Distributed fault-tolerant yahtzee tournament system
%% @author Cory Pruce, Colin Stanfill
%% @doc _D157R18U73D_Y4H7533_
-module(tournament_manager).

-import(shared).
%% ====================================================================
%%                             Public API
%% ====================================================================
-export([tournament_start/2]).
%% ====================================================================
%%                             Constants
%% ====================================================================
%% ====================================================================
%%                            Main Function
%% ====================================================================

% Run a full tournament with the given list of players, reporting results
% back to the Parent pid.
tournament_start(Players, Parent) ->
    halt().
