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
-export([serve_game/6]).
%% ====================================================================
%%                             Constants
%% ====================================================================
%% ====================================================================
%%                             Functions
%% ====================================================================

serve_game(Parent, P1, bye, Tid, Mid, NumGame)->
	log("Player ~p has a bye in match ~p in tournament ~p.~n", [P1, Mid, Tid]),
	Parent ! {match_over, P1, bye, Mid, Tid};
serve_game(Parent, P1, P2, Tid, Mid, NumGame)->
	log("Beginning game between ~p and ~p.~n", [P1, P2]),
    turn(Parent, 1, P1, P2, [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1], [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1], [], [], Tid, Mid, true).

get_score([])-> 0;
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
            tie;
		false ->
			case P1Score > P2Score of
				true ->
					% P1 is the winner
					log("P1 won!~n"),
                    {ok, P1};
				false ->
					% P2 is the winner
					log("P2 won!~n"),
                    {ok, P2}
			end
	end.


% 13 rounds for 1 game
turn(Parent, Round, P1, P2, P1ScoreCard, P2ScoreCard, P1Patterns, P2Patterns, Tid, Mid, IsStandard)->
	log("Turn ~p between ~p and ~p.~n", [Round, P1, P2]),
    Dice1 = [crypto:rand_uniform(1, 7) || _X <- lists:seq(1, 15)],
    Dice2 = case IsStandard of
        true ->
            % Tie game, give different dice.
            [crypto:rand_uniform(1, 7) || _X <- lists:seq(1, 15)];
        false ->
            Dice1
    end,
	{P1Box, P1Pattern} = assembly_phase(P1, P1ScoreCard, P2ScoreCard, [], Tid, Mid, 1, Dice1),
	{P2Box, P2Pattern} = assembly_phase(P2, P2ScoreCard, P1ScoreCard, [], Tid, Mid, 1, Dice2),
 	NewP1SC = replace_nth(P1Pattern, 1, P1Box, length(P1ScoreCard), P1ScoreCard),
    NewP2SC = replace_nth(P2Pattern, 1, P2Box, length(P2ScoreCard), P2ScoreCard),	
    case Round of
        13 ->
            check_winner(Parent, P1, P2, NewP1SC, NewP2SC, Tid, Mid);
        _ ->
            turn(Parent, Round+1, P1, P2, NewP1SC, NewP2SC, P1Patterns++[{P1Box, P1Pattern}], P2Patterns++[{P2Box, P2Pattern}], Tid, Mid, IsStandard)
    end.

replace_nth(N, N, Elem, _ListLength, List)->
	[Elem]++tl(List);
replace_nth(N, M, Elem, ListLength, List)->
	case N >= ListLength of 
		true ->
			error;
		false ->
			replace_nth(N, M+1, Elem, ListLength, tl(List))
	end.
% separate dice kept and the dice to be rerolled
prune_dice([], []) -> [];
prune_dice(Dice, [true | Keepers]) ->
	[hd(Dice)] ++ prune_dice(tl(Dice), Keepers);
prune_dice(Dice, [false | Keepers]) ->
	prune_dice(tl(Dice), Keepers).

assembly_phase(Player, ScoreCard, OppScoreCard, Tid, Mid, Dice, RollNum, Rolls) ->
    ok.
% extra yahtzee bonus of 100 points if 50 points have already been scored for that yahtzee pattern

% emulate random roll of five dice and then two modification attempts
%% assembly_phase(Player, ScoreCard, OppScoreCard, Patterns, Dice, Tid, Mid, RollNum, 6) ->
%%         % 2nd modification attempt
%% 	Player ! {play_request, self(), {make_ref(), Tid, Mid, RollNum, Dice, ScoreCard, OppScoreCard}},
%% 	receive
%% 		{play_action, PlayerPid, {Ref, Tid, Mid, RollNum, Keepers, ScoreCardLine}}->
%% 		        DiceKept = prune_dice(Dice, Keepers),
%% 			SubsetExcl = Dice--DiceKept,	
%% 			NewDice = lists:sort(DiceKept++reroll(SubsetExcl)), 
%% 			case ScoreCardLine > 0 of
%% 				true ->
%% 					log("Player ~p decided to stay on ~p.", [Player, Dice]),
%%                     % FIXME: select line
%%                     {scoring_phase(Dice, yahtzee, ScoreCard), Dice};
%% 				false ->
%%                     % Shouldn't we do one more?
%%                     {scoring_phase(Dice, yahtzee, ScoreCard), Dice};
%% 			end;
%% 		Other -> 
%% 			log("Unexpected message at roll ~p.", [Other])
%% 	end;
%% assembly_phase(Player, ScoreCard, OppScoreCard, Patterns, Dice, Tid, Mid, RollNum, 5) ->
%%         % 1st modification attempt
%% 	SortedDice = lists:sort(Dice),	
%% 	Player ! {play_request, self(), {make_ref(), Tid, Mid, RollNum, SortedDice, ScoreCard, OppScoreCard}},
%% 	receive
%% 		{play_action, PlayerPid, {Ref, Tid, Mid, RollNum, Keepers, ScoreCardLine}}->
%%             DiceKept = prune_dice(Dice, Keepers),
%% 			SubsetExcl = Dice--DiceKept,	
%% 			NewDice = lists:sort(DiceKept++reroll(SubsetExcl)),
%% 			case ScoreCardLine > 0 of
%% 				true ->
%% 					log("Player ~p decided to stay on ~p.~n", [Player, Dice]),
%%                     % FIXME: select line
%%                     {scoring_phase(Dice, yahtzee, ScoreCard), Dice};
%% 				false ->
%% 					assembly_phase(Player, ScoreCard, OppScoreCard, Patterns, NewDice, Tid, Mid, RollNum, 6)
%% 			end;
%% 		Other -> 
%% 			log("Unexpected message at roll ~p. ~n", [Other])
%% 	end;
%% assembly_phase(Player, ScoreCard, OppScoreCard, Patterns, Dice, Tid, Mid, RollNum, DieRoll)->
%% 	% Generate random roll 1 <= N < 7
%% 	Rnd = crypto:rand_uniform(1, 7),
%% 	assembly_phase(Player, ScoreCard, OppScoreCard, Patterns, [Rnd]++Dice, Tid, Mid, RollNum, DieRoll+1).


% Score a roll, given the requested line
% any upper section.
scoring_phase(Xs, {upper, X}, _ScoreCard) ->
    N = length([Y || Y <- Xs, Y == X]),
    log("Upper section ~p. ~p ~ps gives you ~p.", 
        [X, Xs, N, X, X * N]),
    X * N;
scoring_phase([X, X, X, X, X], yahtzee, _ScoreCard ) -> 
	% Yahtzee
	log("Yahtzee! Mark it 50.~n"),
    50;
	%% {50, [X, X, X, X, X]};
scoring_phase([A, B, C, D, E], lstraight, _ScoreCard) when (E == D + 1) and (D == C + 1) and
		(C == B + 1) and (B == A + 1) ->
	% Large Straight
	log("Large straight! That's 40.~n"),
    40;
	%% {40, [A, B, C, D, E]};
scoring_phase([A, B, C, D, E], sstraight, _ScoreCard) when (E == D + 1) and (D == C + 1) and
		(C == B + 1) ->
	log("Small straight! 30 isn't too small relatively.~n"),
    30;
	%% {30, [A, B, C, D, E]};
scoring_phase([A, B, C, D, E], sstraight, _ScoreCard) when (D == C + 1) and (C == B + 1) and 
		(B == A + 1) ->
	log("Small straight! 30 isn't too small relatively.~n"),
    30;
	%% {30, [A, B, C, D, E]};
scoring_phase([A, A, A, B, B], fullhouse, _ScoreCard) when (A /= B)-> 
	% Full House 
	log("Full house. 25 shall be given.~n"),
    25;
	%% {25, [A, A, A, B, B]};
scoring_phase([B, B, A, A, A], fullhouse, _ScoreCard) when (A /= B) ->
	% Full House
	log("Full house. 25 shall be given.~n"),
    25;
	%% {25, [B, B, A, A, A]};
scoring_phase([X, X, X, X, Y], four, _ScoreCard) ->
	Sum = 4 * X + Y,
	log("Four of a kind. Sum is ~p.~n", [Sum]),	
    Sum;
	%% {Sum, [X, X, X, X, Y]};
scoring_phase([Y, X, X, X, X], four, _ScoreCard) ->
	Sum = 4 * X + Y,
	log("Four of a kind. Sum is ~p.~n", [Sum]),	
    Sum;
	%% ({Sum, [Y, X, X, X, X]};
scoring_phase([X, X, X, Y, Z], three, _ScoreCard) ->
	Sum = 3 * X + Y + Z,
	log("Three of a kind. Sum is ~p.~n", [Sum]),	
    Sum;
	%% ({Sum, [X, X, X, Y, Z]};
scoring_phase([Y, X, X, X, Z], three, _ScoreCard) ->
	Sum = 3 * X + Y + Z,
	log("Three of a kind. Sum is ~p.~n", [Sum]),	
    Sum;
	%% ({Sum, [Y, X, X, X, Z]};
scoring_phase([Y, Z, X, X, X], three, _ScoreCard) ->
	Sum = 3 * X + Y + Z,
	log("Three of a kind. Sum is ~p.~n", [Sum]),	
    Sum;
	%% ({Sum, [Y, Z, X, X, X]};
scoring_phase([Y, Z, W, X, X], two, _ScoreCard) ->
	Sum = 2 * X + W + Y + Z,
	log("Two of a kind. Sum is ~p.~n", [Sum]),	
    Sum;
	%% ({Sum, [Y, Z, W, X, X]};
scoring_phase([Y, Z, X, X, W], two, _ScoreCard) ->
	Sum = 2 * X + W + Y + Z,
	log("Two of a kind. Sum is ~p.~n", [Sum]),	
    Sum;
	%% ({Sum, [Y, Z, X, X, W]};
scoring_phase([Y, X, X, W, Z], two, _ScoreCard) ->
	Sum = 2 * X + W + Y + Z,
	log("Two of a kind. Sum is ~p.~n", [Sum]),	
    Sum;
	%% ({Sum, [Y, X, X, W, Z]};
scoring_phase([X, X, W, Y, Z], two, _ScoreCard) ->
	Sum = 2 * X + W + Y + Z,
	log("Two of a kind. Sum is ~p.~n", [Sum]),	
    Sum;
	%% ({Sum, [X, X, W, Y, Z]};
scoring_phase([R1, R2, R3, R4, R5], chance, _ScoreCard)->
	Sum = R1 + R2 + R3 + R4 + R5,
	log("Chance. Sum is ~p.~n", [Sum]),
    Sum;
scoring_phase(_, Pattern, _) ->
    log("Did not match ~p with pattern.", [Pattern]),
    0.

%extra_yahtzee_bonus(_, [], ScoreCard, [])-> [];
%extra_yahtzee_bonus({Sum, [A, B, C, D, E]}, [], ScoreCard, Acc)->
%	extra_yahtzee_bonus(hd(Acc), tl(Acc), ScoreCard, []);
%extra_yahtzee_bonus({Sum, [A, B, C, D, E]}, [{Sum, [A, B, C, D, E]}|Patterns], ScoreCard, Acc)->
%	case Sum >= 50 of
%		true ->
%			extra_yahtzee_bonus(hd(Acc), tl(Acc)++Patterns;
%		false ->
%			{Sum, [A, B, C, D, E]}
%	end;
%extra_yahtzee_bonus({Sum, [A, B, C, D, E]}, Patterns, ScoreCard, Acc)->
%	extra_yahtzee_bonus({Sum, [A, B, C, D, E]}, tl(Patterns), ScoreCard, Acc++[hd(Patterns)]).
