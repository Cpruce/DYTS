%% CSCI182E - Distributed Systems
%% Harvey Mudd College
%% Distributed fault-tolerant yahtzee tournament system
%% @author Cory Pruce, Colin Stanfill
%% @doc _D157R18U73D_Y4H7533_
-module(yahtzee_player).

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
	log("No Tournament Managers to connect to~n", []);	
player_begin(Name, Pwd, TMs)->
	hd(TMs) ! {login, self(), {Name, Pwd}},
	receive
		{logged_in, Pid, LoginTicket}->
			logged_in(Name, Pwd, TMs, [], LoginTicket);
		_ -> 
			player_begin(Name, Pwd, tl(TMs))
	end.

% Messages sent to a logged-in player.
logged_in(Name, Pwd, TMs, Tournaments, ScoreCards, LoginTicket)->
	receive
		% Data is a single tournament identifier
		{start_tournament, Pid, Tid} ->
			log("Received notification that tournament ~p is starting.~n", [Tid]),
			logged_in(Name, Pwd, TMs, Tournaments++[Tid], LoginTicket);
		{end_tournament, Pid, Tid} ->
			log("Received notification that tournament ~p is ending.~n", [Tid]),
			logged_in(Name, Pwd, TMs, Tournaments--[Tid], LoginTicket);
		{play_request, Pid, {Ref, Tid, Gid, RollNum, Dice, ScoreCard, OppScoreCard}}->
			log("Received roll of ~p on roll ~p in game ~p in tournament ~p. Player has ~p and opponent has ~p.~n", [Dice, RollNum, Gid, Tid, Scorecard, OppScoreCard]),
			Keepers = choose_keepers(lists:sort(Dice)),
			ScoreCardLine = choose_line(ScoreCard, Dice, Keepers),
			Pid ! {play_action, self(), {make_ref(), Tid, Gid, RollNum, Keepers, ScoreCardLine}},
			logged_in(Name, Pwd, TMs, Tournaments, LoginTicket);

		{game_over, Pid, } ->
			{Box, Pattern} = find_max(lists:map(fun(X)->scoring_process(X) end, pred_perms(Dice, fun shared:pred/1))),
			NewSC = ScoreCard++{Box, Pattern}, 
			logged_in(Name, Pwd, TMs, Tournaments, ScoreCards, LoginTicket); 
		_ -> 
			log("Received  unknown message")
	end.


% Returns list of booleans (true or false atoms)
% Assumes that a sorted list was supplied
choose_keepers([X, X, X, X, X]) -> 
	% Yahtzee
	log("Yahtzee! Keep everything.~n"),
	[true, true, true, true, true];
choose_keepers([A, B, C, D, E]) when E == D + 1 and D == C + 1 and
		C == B + 1 and B == A + 1 ->
	% Large Straight
	log("Large straight! Keep everything.~n"),
	[true, true, true, true, true];
choose_keepers([A, B, C, D, E]) when E == D + 1 and D == C + 1 and
		C == B + 1 ->
	% Small Straight, might as well go for the Large Straight
	log("Small straight! Keep everything except the first entry.~n"),
	[false, true, true, true, true];
choose_keepers([A, B, C, D, E]) when D == C + 1 and C == B + 1 and 
		B == A + 1 ->
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

% NEED TO DO, return an integer representing a line on the scorecard in
% which to score the dice.
choose_line(ScoreCard, Dice, Keepers)->ok.
