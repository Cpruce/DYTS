%% CSCI182E - Distributed Systems
%% Harvey Mudd College
%% Distributed fault-tolerant yahtzee tournament system
%% @author Cory Pruce, Colin Stanfill
%% @doc _D157R18U73D_Y4H7533_
-module(game_manager).

-import(shared).
%% ====================================================================
%%                             Public API
%% ====================================================================
-export(serve_game/2).
%% ====================================================================
%%                             Constants
%% ====================================================================
%% ====================================================================
%%                             Functions
%% ====================================================================

serve_game(P1, P2, Tid, Gid)->
	log("Beginning game between ~p and ~p.~n", [P1, P2]),
	turn(0, P1, P2, [], [], Tid, Gid).

turn(13, P1, P2, P1ScoreCard, P2ScoreCard, Tid, Gid)->
	log("Last turn between ~p and ~p.~n", [P1, P2]),
	NewP1SC = assembly_phase(P1, P1ScoreCard, P2ScoreCard, [], Tid, Gid, 13, 0),
	NewP2SC = assembly_phase(P2, P2ScoreCard, P1ScoreCard, [], Tid, Gid, 13, 0),
	% Game over, give both players their scorecards
	P1 ! {game_over, self(), {make_ref(), Tid, Gid, NewP1SC, NewP2SC}},
	receive
		{player_exit, P1Pid, {Ref, Tid, Gid}}-> 
			log("Player 1 has left the game.~n"),
			ok;
		_ -> ok
	end,
	P2 ! {game_over, self(), {make_ref(), Tid, Gid, NewP2SC, NewP1SC}},
	receive
		{player_exit, P2Pid, {Ref, Tid, Gid}}-> 
			log("Player 2 has left the game. Game ending.~n"),
			ok;
		_ -> ok
	end;
turn(RollNum, P1, P2, P1ScoreCard, P2ScoreCard, Tid, Gid)->
	log("Turn ~p between ~p and ~p.~n", [RollNum, P1, P2]),
	NewP1SC = assembly_phase(P1, P1ScoreCard, P2ScoreCard, [], Tid, Gid, RollNum, 0),
	NewP2SC = assembly_phase(P2, P2ScoreCard, P1ScoreCard, [], Tid, Gid, RollNum, 0),
	% round over, give both players both scorecards
	P1 ! {turn_over, self(), {make_ref(), Tid, Gid, NewP1SC, NewP2SC}},
	receive
		{player_ready, P1Pid, {Ref, Tid, Gid}}-> 
			log("Player 1 has received the scorecards.~n"),
			ok;
		_ -> ok
	end,
	P2 ! {game_over, self(), {make_ref(), Tid, Gid, NewP2SC, NewP1SC}},
	receive
		{player_exit, P2Pid, {Ref, Tid, Gid}}-> 
			log("Player 2 has received the scorecards.~n"),
			ok;
		_ -> ok
	end,
	turn(RollNum+1, P1, P2, NewP1SC, NewP2SC, Tid, Gid).

% separate dice kept and the dice to be rerolled
prune_dice([], []) -> [];
prune_dice(Dice, [true | Keepers]) ->
	[hd(Dice)] ++ prune_dice(tl(Dice), Keepers);
prune_dice(Dice, [false | Keepers]) ->
	prune_dice(tl(Dice), Keepers).

% emulate random roll of five dice and then two modification attempts
assembly_phase(Player, ScoreCard, OppScoreCard, Dice, Tid, Gid, RollNum, 6) ->
        % 2nd modification attempt
	Player ! {play_request, self(), {make_ref(), Tid, Gid, RollNum, Dice, ScoreCard, OppScoreCard}},
	receive
		{play_action, PlayerPid, {Ref, Tid, Gid, RollNum, Keepers, ScoreCardLine}}->
		        DiceKept = prune_dice(Dice, Keepers),
			SubsetExcl = SortedDice--DiceKept,	
			% mark scorecard!
			NewDice = lists:sort(DiceKept++reroll(SubsetReroll)),
			scoring_process(Player, ScoreCard, OppScoreCard, NewDice, Tid, Gid, RollNum);
		_ -> 
			log("Unexpected message at roll")
	end;
assembly_phase(Player, ScoreCard, OppScoreCard, Dice, Tid, Gid, RollNum, 5) ->
        % 1st modification attempt
	SortedDice = lists:sort(Dice),	
	Player ! {play_request, self(), {make_ref(), Tid, Gid, RollNum, SortedDice, ScoreCard, OppScoreCard}},
	receive
		{play_action, PlayerPid, {Ref, Tid, Gid, RollNum, Keepers, ScoreCardLine}}->
		        DiceKept = prune_dice(SortedDice, Keepers),
			SubsetReroll = SortedDice--DiceKept,	
			% mark scorecard!
			NewDice = lists:sort(DiceKept++reroll(SubsetReroll)),
			assembly_phase(Player, ScoreCard, OppScoreCard, NewDice, Tid, Gid, RollNum, 6);
		_ -> 
			log("Unexpected message at roll")
	end;
assembly_phase(Player, ScoreCard, OppScoreCard, Dice, Tid, Gid, Round)->
	% Generate random roll 1 <= N < 7
	Rnd = crypto:rand_uniform(1, 7),
	assembly_phase(Player, [Rnd]++Dice, Round+1).

% reroll attempts
reroll([]) -> [];
reroll(Subset) ->
	[crypto:rand_uniform(1, 7)] ++ reroll(tl(Subset)).	

% scoring phase, return the updated scorecard
scoring_phase(Player, ScoreCard, OppScoreCard, Dice, Tid, Gid, RollNum)->
	log("Beginning scoring phase for ~p in round ~p in game ~p in tournament ~p.~n", [Player, RollNum, Gid, Tid]),
	find_max(lists:map(fun(X)->scoring_process(X) end, pred_perms(

% returns the max score of a pattern
scoring_process([X, X, X, X, X]) -> 
	% Yahtzee
	log("Yahtzee! Keep everything.~n"),
	[true, true, true, true, true];
scoring_process([A, B, C, D, E]) when E == D + 1 and D == C + 1 and
		C == B + 1 and B == A + 1 ->
	% Large Straight
	log("Large straight! Keep everything.~n"),
	[true, true, true, true, true];
scoring_process([A, B, C, D, E]) when E == D + 1 and D == C + 1 and
		C == B + 1 ->
	% Small Straight, might as well go for the Large Straight
	log("Small straight! Keep everything except the first entry.~n"),
	[false, true, true, true, true];
scoring_process([A, B, C, D, E]) when D == C + 1 and C == B + 1 and 
		B == A + 1 ->
	% Small Straight, might as well go for the Large Straight
	log("Small straight! Keep everything except the last entry.~n"),
	[true, true, true, true, false];	
scoring_process([A, A, A, B, B]) -> 
	% Full House 
	log("Full house. Keep everything, not worth the risk.~n"),
	[true, true, true, true, true];
scoring_process([B, B, A, A, A]) ->
	% Full House
	log("Full house. Keep everything, not worth the risk.~n"),
	[true, true, true, true, true];
scoring_process([X, X, X, X, Y]) ->
	% Four of a Kind, might as well go for the Yahtzee
	log("Four of a kind. Keep everything except the last entry.~n"),	
	[true, true, true, true, false];
scoring_process([Y, X, X, X, X]) ->
	% Four of a Kind, might as well go for the Yahtzee
	log("Four of a kind. Keep everything except the first entry.~n"),	
	[false, true, true, true, true];
scoring_process([X, X, X, Y, Z]) ->
	% Three of a kind, shoot for four of a kind or Yahtzee
	log("Three of a kind. Keep everything except the last two entries.~n"),	
	[true, true, true, false, false];
scoring_process([Y, X, X, X, Z]) ->
	% Three of a kind, shoot for four of a kind or Yahtzee
	log("Three of a kind. Keep everything except the first entry and the last entry.~n"),
	[false, true, true, true, false];
scoring_process([Y, Z, X, X, X]) ->
	% Three of a kind, shoot for four of a kind or Yahtzee
	log("Three of a kind. Keep everything except the first two entries.~n"),
	[false, false, true, true, true];
scoring_process([Y, Z, W, X, X]) ->
	% Two of a kind, shoot for more
	log("Two of a kind. Keep the pair.~n"),
	[false, false, false, true, true];
scoring_process([Y, Z, X, X, W]) ->
	% Two of a kind, shoot for more
	log("Two of a kind. Keep the pair.~n"),
	[false, false, true, true, false];
scoring_process([Y, X, X, W, Z]) ->
	% Two of a kind, shoot for more
	log("Two of a kind. Keep the pair.~n"),
	[false, true, true, false, false];
scoring_process([X, X, W, Y, Z]) ->
	% Two of a kind, shoot for more
	log("Two of a kind. Keep the pair.~n"),
	[true, true, false, false, false];
scoring_process([R1, R2, R3, R4, R5])->
	% No pattern
	log("No pattern, get new set of 5"),
	[false, false, false, false, false].
	
