%% CSCI182E - Distributed Systems
%% Harvey Mudd College
%% Distributed fault-tolerant yahtzee tournament system
%% @author Cory Pruce, Colin Stanfill
%% @doc _D157R18U73D_Y4H7533_
-module(game_manager).

-import(shared, [pred/1, pred_perms/2, timestamp/0, log/1, log/2, shuffle/1]).
%% ====================================================================
%%                             Public API
%% ====================================================================
-export([serve_game/5]).
%% ====================================================================
%%                             Constants
%% ====================================================================
%% ====================================================================
%%                             Functions
%% ====================================================================

serve_game(Parent, P1, bye, Tid, Mid)->
	log("Player ~p has a bye in match ~p in tournament ~p.~n", [P1, Mid, Tid]),
	Parent ! {match_over, P1, bye, Mid, Tid};
serve_game(Parent, P1, P2, Tid, Mid)->
	log("Beginning game between ~p and ~p.~n", [P1, P2]),
	turn(Parent, 0, P1, P2, [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0], [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0], Tid, Mid).

get_score([_X])-> 0;
get_score([Box|ScoreCard]) ->
	Box+get_score(ScoreCard).

% Game over, check winner
check_winner(Parent, P1, P2, P1ScoreCard, P2ScoreCard, Tid, Mid)->
	P1Score = get_score(P1ScoreCard),
	P2Score = get_score(P2ScoreCard),
	case P1Score == P2Score of
		true ->
			% Tie game, start game over
			log("Tie game, starting over.~n"),
			turn(Parent, 0, P1, P2, [], [], Tid, Mid);
		false ->
			case P1Score > P2Score of
				true ->
					% P1 is the winner
					log("P1 won!~n"),
					Parent ! {match_over, P1, P2, Mid, Tid};
				false ->
					% P2 is the winner
					log("P2 won!~n"),
					Parent ! {match_over, P1, P2, Mid, Tid}
			end
	end.


% 13 rounds for 1 game
turn(Parent, 13, P1, P2, P1ScoreCard, P2ScoreCard, Tid, Mid)->
	log("Last turn between ~p and ~p.~n", [P1, P2]),
	{P1Box, _P1Pattern} = assembly_phase(P1, P1ScoreCard, P2ScoreCard, [], Tid, Mid, 13, 0),
	{P2Box, _P2Pattern} = assembly_phase(P2, P2ScoreCard, P1ScoreCard, [], Tid, Mid, 13, 0),
	NewP1SC = replace_nth(13, 1, P1Box, length(P1ScoreCard), P1ScoreCard),
        NewP2SC = replace_nth(13, 1, P2Box, length(P2ScoreCard), P2ScoreCard),	
	check_winner(Parent, P1, P2, NewP1SC, NewP2SC, Tid, Mid);
turn(Parent, RollNum, P1, P2, P1ScoreCard, P2ScoreCard, Tid, Mid)->
	log("Turn ~p between ~p and ~p.~n", [RollNum, P1, P2]),
	{P1Box, _P1Pattern} = assembly_phase(P1, P1ScoreCard, P2ScoreCard, [], Tid, Mid, RollNum, 0),
	{P2Box, _P2Pattern} = assembly_phase(P2, P2ScoreCard, P1ScoreCard, [], Tid, Mid, RollNum, 0),
 	NewP1SC = replace_nth(RollNum, 1, P1Box, length(P1ScoreCard), P1ScoreCard),
        NewP2SC = replace_nth(RollNum, 1, P2Box, length(P2ScoreCard), P2ScoreCard),	
	turn(Parent, RollNum+1, P1, P2, NewP1SC, NewP2SC, Tid, Mid).

replace_nth(n, n, elem, _ListLength, List)->
	[elem]++tl(List);
replace_nth(n, m, elem, ListLength, List)->
	case n >= ListLength of 
		true ->
			error;
		false ->
			replace_nth(n, m+1, elem, ListLength, tl(List))
	end.
% separate dice kept and the dice to be rerolled
prune_dice([], []) -> [];
prune_dice(Dice, [true | Keepers]) ->
	[hd(Dice)] ++ prune_dice(tl(Dice), Keepers);
prune_dice(Dice, [false | Keepers]) ->
	prune_dice(tl(Dice), Keepers).

% emulate random roll of five dice and then two modification attempts
assembly_phase(Player, ScoreCard, OppScoreCard, Dice, Tid, Mid, RollNum, 6) ->
        % 2nd modification attempt
	Player ! {play_request, self(), {make_ref(), Tid, Mid, RollNum, Dice, ScoreCard, OppScoreCard}},
	receive
		{play_action, PlayerPid, {Ref, Tid, Mid, RollNum, Keepers, ScoreCardLine}}->
		        DiceKept = prune_dice(Dice, Keepers),
			SubsetExcl = Dice--DiceKept,	
			NewDice = lists:sort(DiceKept++reroll(SubsetExcl)), 
			case ScoreCardLine > 0 of
				true ->
					log("Player ~p decided to stay on ~p.~n", [Player, Dice]),
					scoring_phase(NewDice);
				false ->
					scoring_phase(NewDice)
			end;
		Other -> 
			log("Unexpected message at roll ~p. ~n", [Other])
	end;
assembly_phase(Player, ScoreCard, OppScoreCard, Dice, Tid, Mid, RollNum, 5) ->
        % 1st modification attempt
	SortedDice = lists:sort(Dice),	
	Player ! {play_request, self(), {make_ref(), Tid, Mid, RollNum, SortedDice, ScoreCard, OppScoreCard}},
	receive
		{play_action, PlayerPid, {Ref, Tid, Mid, RollNum, Keepers, ScoreCardLine}}->
		        DiceKept = prune_dice(Dice, Keepers),
			SubsetExcl = Dice--DiceKept,	
			NewDice = lists:sort(DiceKept++reroll(SubsetExcl)),
			case ScoreCardLine > 0 of
				true ->
					log("Player ~p decided to stay on ~p.~n", [Player, Dice]),
					scoring_phase(NewDice);	
				false ->
					assembly_phase(Player, ScoreCard, OppScoreCard, NewDice, Tid, Mid, RollNum, 6)
			end;
		Other -> 
			log("Unexpected message at roll ~p. ~n", [Other])
	end;
assembly_phase(Player, ScoreCard, OppScoreCard, Dice, Tid, Mid, RollNum, DieRoll)->
	% Generate random roll 1 <= N < 7
	Rnd = crypto:rand_uniform(1, 7),
	assembly_phase(Player, ScoreCard, OppScoreCard, [Rnd]++Dice, Tid, Mid, RollNum, DieRoll+1).

% reroll attempts
reroll([]) -> [];
reroll(Subset) ->
	[crypto:rand_uniform(1, 7)] ++ reroll(tl(Subset)).	


% Returns a tuple of the Box filled in with the score 
% and the scorecard updated with the newest pattern.
scoring_phase([X, X, X, X, X]) -> 
	% Yahtzee
	log("Yahtzee! Mark it 50.~n"),
	{50, [X, X, X, X, X]};
scoring_phase([A, B, C, D, E]) when (E == D + 1) and (D == C + 1) and
		(C == B + 1) and (B == A + 1) ->
	% Large Straight
	log("Large straight! That's 40.~n"),
	{40, [A, B, C, D, E]};
scoring_phase([A, B, C, D, E]) when (E == D + 1) and (D == C + 1) and
		(C == B + 1) ->
	log("Small straight! 30 isn't too small relatively.~n"),
	{30, [A, B, C, D, E]};
scoring_phase([A, B, C, D, E]) when (D == C + 1) and (C == B + 1) and 
		(B == A + 1) ->
	log("Small straight! 30 isn't too small relatively.~n"),
	{30, [A, B, C, D, E]};
scoring_phase([A, A, A, B, B]) -> 
	% Full House 
	log("Full house. 25 shall be given.~n"),
	{25, [A, A, A, B, B]};
scoring_phase([B, B, A, A, A]) ->
	% Full House
	log("Full house. 25 shall be given.~n"),
	{25, [B, B, A, A, A]};
scoring_phase([X, X, X, X, Y]) ->
	Sum = 4 * X + Y,
	log("Four of a kind. Sum is ~p.~n", [Sum]),	
	{Sum, [X, X, X, X, Y]};
scoring_phase([Y, X, X, X, X]) ->
	Sum = 4 * X + Y,
	log("Four of a kind. Sum is ~p.~n", [Sum]),	
	{Sum, [Y, X, X, X, X]};
scoring_phase([X, X, X, Y, Z]) ->
	Sum = 3 * X + Y + Z,
	log("Three of a kind. Sum is ~p.~n", [Sum]),	
	{Sum, [X, X, X, Y, Z]};
scoring_phase([Y, X, X, X, Z]) ->
	Sum = 3 * X + Y + Z,
	log("Three of a kind. Sum is ~p.~n", [Sum]),	
	{Sum, [Y, X, X, X, Z]};
scoring_phase([Y, Z, X, X, X]) ->
	Sum = 3 * X + Y + Z,
	log("Three of a kind. Sum is ~p.~n", [Sum]),	
	{Sum, [Y, Z, X, X, X]};
scoring_phase([Y, Z, W, X, X]) ->
	Sum = 2 * X + W + Y + Z,
	log("Two of a kind. Sum is ~p.~n", [Sum]),	
	{Sum, [Y, Z, W, X, X]};
scoring_phase([Y, Z, X, X, W]) ->
	Sum = 2 * X + W + Y + Z,
	log("Two of a kind. Sum is ~p.~n", [Sum]),	
	{Sum, [Y, Z, X, X, W]};
scoring_phase([Y, X, X, W, Z]) ->
	Sum = 2 * X + W + Y + Z,
	log("Two of a kind. Sum is ~p.~n", [Sum]),	
	{Sum, [Y, X, X, W, Z]};
scoring_phase([X, X, W, Y, Z]) ->
	Sum = 2 * X + W + Y + Z,
	log("Two of a kind. Sum is ~p.~n", [Sum]),	
	{Sum, [X, X, W, Y, Z]};
scoring_phase([R1, R2, R3, R4, R5])->
	Sum = R1 + R2 + R3 + R4 + R5,
	log("No pattern. Sum is ~p.~n", [Sum]),
	{Sum, [R1, R2, R3, R4, R5]}.


