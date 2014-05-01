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
    Parent ! {request_players, NumPlayers},
    tournament_wait(Parent, Tid, NumPlayers, Gpm, [], []).


tournament_wait(Parent, Tid, NumPlayers, Gpm, Players, Pending) ->
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
                    Parent ! {request_players, 1},
                    Pending_ = remove_user(Username, Pending),
                    tournament_wait(Parent, Tid, NumPlayers, Gpm, Players, Pending_);
                error ->
                    % no such user
                    Pid ! {end_tournament, Parent, Username, Tid},
                    Parent ! {request_players, 1},
                    tournament_wait(Parent, Tid, NumPlayers, Gpm, Players, Pending)
            end
    after 1000 ->
            Parent ! {request_players, NumPlayers - length(Players)},
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
