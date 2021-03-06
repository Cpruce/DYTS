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

bracket_setup(Parent, Tid, Mid, NumPlayers, Gpm, [], Bracket)->
	tournament_run(Parent, Tid, Mid, NumPlayers, Gpm, Bracket, []);
bracket_setup(Parent, Tid, Mid, NumPlayers, Gpm, [P1], Bracket)->
	tournament_run(Parent, Tid, Mid, NumPlayers, Gpm, Bracket++[{P1, bye}], []);
bracket_setup(Parent, Tid, Mid, NumPlayers, Gpm, [P1,P2|Players], Bracket)->
	bracket_setup(Parent, Tid, Mid, NumPlayers, Gpm, Players, Bracket++[{P1, P2}]).

tournament_run(Parent, Tid, Mid, NumPlayers, Gpm, [], Winners)->
	case length(Winners) == 1 of
		true ->
			log("Tournament over, the winner is ~p!", [hd(Winners)]),
			case hd(Winners) of
                {_, Pid_, _} ->
                    Pid_ ! {end_tournament, Tid};
                bye ->
                    ok
            end,
			Parent ! {tournament_complete, Tid, hd(Winners)},
            exit(self());
		false ->
			log("")
	end,
	case length(Winners) == NumPlayers of
		true ->
			log("Initiating another round of elimination."),
		    	bracket_setup(Parent, Tid, Mid, NumPlayers, Gpm, Winners, []);
		false ->
			log("Waiting for more matches to end.")
	end,
    log("waiting"),
	receive
		{match_over, Winner, Loser, RMid, Tid}->
			log("~p won against ~p in match ~p in tournament ~p.", [Winner, Loser, RMid, Tid]),
            {LN, LP, _} = Loser,
            {WN, _, _} = Winner,
			LP ! {end_tournament, Tid},
            Parent ! {match_results, self(), WN, LN},
			tournament_run(Parent, Tid, Mid, NumPlayers-1, Gpm, [], Winners++[Winner]);
        {match_fault, Loser1, Loser2, RMid, Tid}->
			log("Both ~p and ~p crashed or cheated too much in match ~p (tournament ~p)", [Loser1, Loser1, RMid, Tid]),
            {LN1, LP1, _} = Loser1,
            {LN2, LP2, _} = Loser2,
			LP1 ! {end_tournament, Tid},
			LP2 ! {end_tournament, Tid},
            Parent ! {double_fault, self(), LN1, LN2},
			tournament_run(Parent, Tid, Mid, NumPlayers-1, Gpm, [], Winners++[bye]);
        {invalidate, Username} ->
            Winners_ = invalidate_user(Winners, Username),
            tournament_run(Parent, Tid, Mid, NumPlayers, Gpm, [], Winners_);
        {back, Username, Pid, Token} ->
            Winners_ = revalidate_user(Winners, Username, Pid, Token),
            tournament_run(Parent, Tid, Mid, NumPlayers, Gpm, [], Winners_);
		Other ->
			log("Received something unparseable. ~p", [Other])
	end;
tournament_run(Parent, Tid, Mid, NumPlayers, Gpm, Bracket, Winners)->
    Pid = self(),
    spawn(fun() -> bracket_run(Parent, Pid, Tid, Mid, Bracket, Gpm) end),
	tournament_run(Parent, Tid, Mid+length(Bracket), NumPlayers, Gpm, [], Winners).	

bracket_run(_Parent, _Pid,  _Tid, _Mid, [], _Gpm)-> [];
bracket_run(Parent, Pid, Tid, Mid, [{P1, bye}|Bracket], Gpm)->
	spawn(game_manager, serve_match, [Pid, P1, bye, Tid, Mid, Gpm]),
	bracket_run(Parent, Pid, Tid, Mid+1, Bracket, Gpm); 
bracket_run(Parent, Pid, Tid, Mid, [{P1, P2}|Bracket], Gpm)->
	spawn(game_manager, serve_match, [Pid, P1, P2, Tid, Mid, Gpm]),
	bracket_run(Parent, Pid, Tid, Mid+1, Bracket, Gpm). 

tournament_wait(Parent, Tid, NumPlayers, Gpm, Players, Pending) ->
    case length(Players) == NumPlayers of
	    true ->
		    log("The required number of players have joined. Tournament ~p about to start.", [Tid]),
            PlayerNames = [Username || {Username, _, _} <- Players],
		    Parent ! {tournament_begin, self(), Tid, PlayerNames},
		    bracket_setup(Parent, Tid, 0, NumPlayers, Gpm, Players, []);
	    false ->
		    log("Waiting for players to join.")
    end,
    receive
        % internal
        {add_players, New} ->
            NewUniq = New -- (Pending ++ Players),
            send_to_each(NewUniq, start_tournament, Tid),
            tournament_wait(Parent, Tid, NumPlayers, Gpm, Players, Pending ++ NewUniq);
        {invalidate, Username} ->
            case is_ready(Players, Username) of
                true ->
                    Players_ = remove_user(Players, Username),
                    Parent ! {request_players, 1},
                    tournament_wait(Parent, Tid, NumPlayers, Gpm, Players_, Pending);
                false ->
                    Pending_ = invalidate_user(Pending, Username),
                    tournament_wait(Parent, Tid, NumPlayers, Gpm, Players, Pending_)
            end;
        {back, Username, Pid, Token} ->
            case is_ready(Pending, Username) of
                true ->
                    Pid ! {start_tournament, self(), Username, Tid},
                    Pending_ = revalidate_user(Pending, Username, Pid, Token),
                    tournament_wait(Parent, Tid, NumPlayers, Gpm, Players, Pending_);
                false ->
                    % we don't even remember this person.
                    tournament_wait(Parent, Tid, NumPlayers, Gpm, Players, Pending)
            end;
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
        false -> bad,
            log("Invalid token for player ~p", [Username]),
            log("Wanted ~p, was ~p", [Token_, Token])

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

invalidate_user([], _) -> [];
invalidate_user([{Username, _, _}|T], Username) ->
    [{Username, none, none}|T];
invalidate_user([H|T], Username) -> [H|invalidate_user(T, Username)].

revalidate_user([], _, _, _) -> [];
revalidate_user([{Username, _, _}|T], Username, Pid, Token) ->
    [{Username, Pid, Token}|T];
revalidate_user([H|T], Username, Pid, Token) -> [H|revalidate_user(T, Username, Pid, Token)].

is_ready([], _) -> false;
is_ready([{Username, _, _}|_], Username) -> true;
is_ready([_|T], Username) -> is_ready(T, Username).
