%% CSCI182E - Distributed Systems
%% Harvey Mudd College
%% Distributed fault-tolerant yahtzee tournament system
%% @author Cory Pruce, Colin Stanfill
%% @doc _D157R18U73D_Y4H7533_
-module(yahtzee_player).

-import(shared, [pred/1, pred_perms/2, timestamp/0, log/1, log/2, shuffle/1]).
%% ====================================================================
%%                             Public API
%% ====================================================================
-export([main/1]).
%% ====================================================================
%%                             Constants
%% ====================================================================
%% ====================================================================
%%                            Main Function
%% ====================================================================
% The main/1 function.
main(Params)->
	% The first parameter is username for registering and starting 
	% up the kernel.
	Name = hd(Params),
	% The second parameter is a password to use when registering.
	Pwd = hd(tl(Params)),
	% The third...nth parameter is a tournament manager name that
	% is globally registered.
	TMs = tl(tl(Params)),
	% IMPORTANT: Start the empd daemon!
        os:cmd("epmd -daemon"),
        % format microseconds of timestamp to get an
        % effectively-unique node name
        net_kernel:start([list_to_atom(Name), shortnames]),
        register(Name, self()),
	player_begin(Name, Pwd, TMs).

% Messages sent to a player.
player_begin(_Name, _Pwd, [])->
	log("No Tournament Managers to connect to. Hang around for a tournament manager.~n", []),
	receive 
		{tournament_manager, Pid, Name} ->
			ok;
		_ ->
			ok
	end;	
player_begin(Name, Pwd, TMs)->
	hd(TMs) ! {login, self(), {Name, Pwd}},
	receive
		{logged_in, Pid, LoginTicket}->
			logged_in(Name, Pwd, TMs, dict:new(), LoginTicket);
		_ -> 
			player_begin(Name, Pwd, tl(TMs))
	end.

% Messages sent to a logged-in player.
logged_in(Name, Pwd, TMs, Tournaments, LoginTicket)->
	receive
		% Data is a single tournament identifier
		{start_tournament, Pid, Tid} ->
			log("Received notification that tournament ~p is starting.~n", [Tid]),
			% create new table of matches
			dict:store(Tid, dict:new(), Tournaments),
			logged_in(Name, Pwd, TMs, Tournaments, LoginTicket);
		{end_tournament, Pid, Tid} ->
			log("Received notification that tournament ~p is ending.~n", [Tid]),

			logged_in(Name, Pwd, TMs, Games, LoginTicket);
		{play_request, Pid, {Ref, Tid, Gid, RollNum, Dice, ScoreCard, OppScoreCard}}->
			log("Received roll of ~p on roll ~p in game ~p in tournament ~p. Player has ~p and opponent has ~p.~n", [Dice, RollNum, Gid, Tid, Scorecard, OppScoreCard]),
			{Box, Pattern} = find_max(lists:map(fun(X)->scoring_process(X) end, pred_perms(Dice, fun shared:pred/1))),
			NKeepers = choose_keepers(lists:sort(Dice)),
			case Box == 50 of 
				true ->
					% no need to continue
					ScoreCardLine = 1;
				false ->
					ScoreCardLine = 0
			end,
			Pid ! {play_action, self(), {make_ref(), Tid, Gid, RollNum, Keepers, ScoreCardLine}},
			logged_in(Name, Pwd, TMs, Tournaments, LoginTicket);

			logged_in(Name, Pwd, TMs, Tournaments, Games++[NewSC], LoginTicket); 
		_ -> 
			log("Received  unknown message")
	end.

% Returns a tuple of the Box filled in with the score 
% and the scorecard updated with the newest pattern.
scoring_process([X, X, X, X, X]) -> 
	% Yahtzee
	log("Yahtzee! Mark it 50.~n"),
	{50, [X, X, X, X, X]};
scoring_process([A, B, C, D, E]) when (E == D + 1) and (D == C + 1) and
		(C == B + 1) and (B == A + 1) ->
	% Large Straight
	log("Large straight! That's 40.~n"),
	{40, [A, B, C, D, E]};
scoring_process([A, B, C, D, E]) when (E == D + 1) and (D == C + 1) and
		(C == B + 1) ->
	log("Small straight! 30 isn't too small relatively.~n"),
	{30, [A, B, C, D, E]};
scoring_process([A, B, C, D, E]) when (D == C + 1) and (C == B + 1) and 
		(B == A + 1) ->
	log("Small straight! 30 isn't too small relatively.~n"),
	{30, [A, B, C, D, E]};
scoring_process([A, A, A, B, B]) -> 
	% Full House 
	log("Full house. 25 shall be given.~n"),
	{25, [A, A, A, B, B]};
scoring_process([B, B, A, A, A]) ->
	% Full House
	log("Full house. 25 shall be given.~n"),
	{25, [B, B, A, A, A]};
scoring_process([X, X, X, X, Y]) ->
	Sum = 4 * X + Y,
	log("Four of a kind. Sum is ~p.~n", [Sum]),	
	{Sum, [X, X, X, X, Y]};
scoring_process([Y, X, X, X, X]) ->
	Sum = 4 * X + Y,
	log("Four of a kind. Sum is ~p.~n", [Sum]),	
	{Sum, [Y, X, X, X, X]};
scoring_process([X, X, X, Y, Z]) ->
	Sum = 3 * X + Y + Z,
	log("Three of a kind. Sum is ~p.~n", [Sum]),	
	{Sum, [X, X, X, Y, Z]};
scoring_process([Y, X, X, X, Z]) ->
	Sum = 3 * X + Y + Z,
	log("Three of a kind. Sum is ~p.~n", [Sum]),	
	{Sum, [Y, X, X, X, Z]};
scoring_process([Y, Z, X, X, X]) ->
	Sum = 3 * X + Y + Z,
	log("Three of a kind. Sum is ~p.~n", [Sum]),	
	{Sum, [Y, Z, X, X, X]};
scoring_process([Y, Z, W, X, X]) ->
	Sum = 2 * X + W + Y + Z,
	log("Two of a kind. Sum is ~p.~n", [Sum]),	
	{Sum, [Y, Z, W, X, X]};
scoring_process([Y, Z, X, X, W]) ->
	Sum = 2 * X + W + Y + Z,
	log("Two of a kind. Sum is ~p.~n", [Sum]),	
	{Sum, [Y, Z, X, X, W]};
scoring_process([Y, X, X, W, Z]) ->
	Sum = 2 * X + W + Y + Z,
	log("Two of a kind. Sum is ~p.~n", [Sum]),	
	{Sum, [Y, X, X, W, Z]};
scoring_process([X, X, W, Y, Z]) ->
	Sum = 2 * X + W + Y + Z,
	log("Two of a kind. Sum is ~p.~n", [Sum]),	
	{Sum, [X, X, W, Y, Z]};
scoring_process([R1, R2, R3, R4, R5])->
	Sum = R1 + R2 + R3 + R4 + R5,
	log("No pattern. Sum is ~p.~n", [Sum]),
	{Sum, [R1, R2, R3, R4, R5]}.

% Returns list of booleans (true or false atoms)
% Assumes that a sorted list was supplied
choose_keepers([X, X, X, X, X]) -> 
	% Yahtzee
	log("Yahtzee! Keep everything.~n"),
	[true, true, true, true, true];
choose_keepers([A, B, C, D, E]) when (E == D + 1) and (D == C + 1) and
		(C == B + 1) and (B == A + 1) ->
	% Large Straight
	log("Large straight! Keep everything.~n"),
	[true, true, true, true, true];
choose_keepers([A, B, C, D, E]) when (E == D + 1) and (D == C + 1) and
		(C == B + 1) ->
	% Small Straight, might as well go for the Large Straight
	log("Small straight! Keep everything except the first entry.~n"),
	[false, true, true, true, true];
choose_keepers([A, B, C, D, E]) when (D == C + 1) and (C == B + 1) and 
		(B == A + 1) ->
	% Small Straight, might as well go for the Large Straight
	log("Small straight! Keep everything except the last entry.~n"),
	[true, true, true, true, false];	
choose_keepers([A, A, A, B, B]) -> 
	% Full House 
	log("Full house. Keep everything, not worth the risk.~n"),
	[true, true, true, true, true];
choose_keepers([B, B, A, A, A]) ->
	% Full House
	log("Full house. Keep everything, not worth the risk.~n"),
	[true, true, true, true, true];
choose_keepers([X, X, X, X, Y]) ->
	% Four of a Kind, might as well go for the Yahtzee
	log("Four of a kind. Keep everything except the last entry.~n"),	
	[true, true, true, true, false];
choose_keepers([Y, X, X, X, X]) ->
	% Four of a Kind, might as well go for the Yahtzee
	log("Four of a kind. Keep everything except the first entry.~n"),	
	[false, true, true, true, true];
choose_keepers([X, X, X, Y, Z]) ->
	% Three of a kind, shoot for four of a kind or Yahtzee
	log("Three of a kind. Keep everything except the last two entries.~n"),	
	[true, true, true, false, false];
choose_keepers([Y, X, X, X, Z]) ->
	% Three of a kind, shoot for four of a kind or Yahtzee
	log("Three of a kind. Keep everything except the first entry and the last entry.~n"),
	[false, true, true, true, false];
choose_keepers([Y, Z, X, X, X]) ->
	% Three of a kind, shoot for four of a kind or Yahtzee
	log("Three of a kind. Keep everything except the first two entries.~n"),
	[false, false, true, true, true];
choose_keepers([Y, Z, W, X, X]) ->
	% Two of a kind, shoot for more
	log("Two of a kind. Keep the pair.~n"),
	[false, false, false, true, true];
choose_keepers([Y, Z, X, X, W]) ->
	% Two of a kind, shoot for more
	log("Two of a kind. Keep the pair.~n"),
	[false, false, true, true, false];
choose_keepers([Y, X, X, W, Z]) ->
	% Two of a kind, shoot for more
	log("Two of a kind. Keep the pair.~n"),
	[false, true, true, false, false];
choose_keepers([X, X, W, Y, Z]) ->
	% Two of a kind, shoot for more
	log("Two of a kind. Keep the pair.~n"),
	[true, true, false, false, false];
choose_keepers([R1, R2, R3, R4, R5])->
	% No pattern
	log("No pattern, get new set of 5"),
	[false, false, false, false, false].
	%case (R2 == R1 + 1 and R3 == R2 + 1) of
	%	or (R2 == R1 + 1 and R4 == R3 + 1) 		or (R2 == R1 + 1 and R.

