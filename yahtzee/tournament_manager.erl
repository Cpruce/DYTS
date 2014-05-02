%% CSCI182E - Distributed Systems
%% Harvey Mudd College
%% Distributed fault-tolerant yahtzee tournament system
%% @author Cory Pruce, Colin Stanfill
%% @doc _D157R18U73D_Y4H7533_
-module(tournament_manager).

-import(shared,[pred/1, pred_perms/2, timestamp/0, log/1, log/2, shuffle/1]).
%% ====================================================================
%%                             Public API
%% ====================================================================
-export([tournament_start/4]).
%% ====================================================================
%%                             Constants
%% ====================================================================
%% ====================================================================
%%                            Main Function
%% ====================================================================

% Run a full tournament with the given list of players, reporting results
% back to the Parent pid.
tournament_start(Parent, Tid, NumPlayers, Gpm) ->
    Parent ! {request_players, self(), NumPlayers},
    tournament_wait(Parent, Tid, NumPlayers, Gpm, [], []).

bracket_setup(Parent, Tid, NumPlayers, Gpm, [], Bracket)->
	tournament_run(Parent, Tid, 1, NumPlayers, Gpm, Bracket);
bracket_setup(Parent, Tid, NumPlayers, Gpm, [P1], Bracket)->
	tournament_run(Parent, Tid, 1, NumPlayers, Gpm, Bracket++[{P1, bye}], 1);
bracket_setup(Parent, Tid, NumPlayers, Gpm, [P1,P2|Players], Bracket)->
	bracket_setup(Parent, Tid, NumPlayers, Gpm, Players, Bracket++[{P1, P2}]).

tournament_run(Parent, Tid, Gid, NumPlayers, Gpm, [])->
	receive
		{}->
			;
		Other ->

	end;
tournament_run(Parent, Tid, Gid, NumPlayers, Gpm, Bracket)->
	spawn(tournament_manager, bracket_run, [Parent, Tid, Gid, Bracket]),
	tournament_run(Parent, Tid, NumPlayers, NumPlayers, Gpm, []).	

bracket_run(Parent, Tid, Gid, [])->
bracket_run(Parent, Tid, Gid, [{P1, bye}|Bracket])->
	%Parent ! {some_message signifying P1 won}
bracket_run(Parent, Tid, Gid, [{P1, P2}|Bracket])->
	spawn(game_manager, serve_game, [P1, P2, Tid, Gid]),
	bracket_run(Parent, Tid, Gid+1, Bracket). 
	%	

tournament_wait(Parent, Tid, NumPlayers, Gpm, Players, Pending) ->
    case length(Pending) == NumPlayers of
	    true ->
		    log("The required number of players have joined"),
		    bracket_setup(Parent, Tid, NumPlayers, Gpm, Pending, []);
	    false ->
		    log("Waiting for players to join.~n")
    end,
    receive
        % internal
        {add_players, New} ->
            send_to_each(New, start_tournament, Tid),
            tournament_wait(Parent, Tid, NumPlayers, Gpm, Players, Pending ++ New);
        % external
        {accept_tournament, Pid, Username, {Tid, Ticket}} ->
            case validate_ticket(Pending, Username, Ticket) of
                ok ->
                    Players_ = [{Username, Pid, Ticket}|Players],
                    Pending_ = remove_user(Username, Pending),
                    tournament_wait(Parent, Tid, NumPlayers, Gpm, Players_, Pending_);
                bad ->
                    Pid ! {end_tournament, Parent, Username, Tid},
		    Parent ! {request_players, self(), 1},
                    Pending_ = remove_user(Username, Pending),
                    tournament_wait(Parent, Tid, NumPlayers, Gpm, Players, Pending_);
                error ->
                    % no such user
                    Pid ! {end_tournament, Parent, Username, Tid},
		    Parent ! {request_players, self(), 1},
                    tournament_wait(Parent, Tid, NumPlayers, Gpm, Players, Pending)
            end
    after 1000 ->
	    Parent ! {request_players, self(), NumPlayers - length(Players)},
            tournament_wait(Parent, Tid, NumPlayers, Gpm, Players, [])
    end.

validate_ticket([], _, _) -> error;
validate_ticket([{Username, _, Token}|_], Username, Token_)  ->
    case (Token == Token_) of
        true -> ok;
        false -> bad
    end;
validate_ticket([_|T], Username, Token)  ->
    validate_ticket(T, Username, Token).

send_to_each([], _, _) -> ok;
send_to_each([{Username, Pid, _}|T], Type, Data) ->
    Pid ! {Type, self(), Username, Data},
    send_to_each(T, Type, Data).

remove_user([], _) -> [];
remove_user([{Username, _, _}|T], Username) -> T;
remove_user([H|T], Username) -> [H|remove_user(T, Username)].
