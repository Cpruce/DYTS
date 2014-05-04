%% CSCI182E - Distributed Systems
%% Harvey Mudd College
%% Distributed fault-tolerant yahtzee tournament system
%% @author Cory Pruce, Colin Stanfill
%% @doc _D157R18U73D_Y4H7533_
-module(game_manager).

-import(shared, [pred/1, pred_perms/2, timestamp/0, log/1, log/2, shuffle/1]).
-define(PATTERNS, {{upper, 1}, {upper, 2}, {upper, 3}, {upper, 4}, {upper, 5}, {upper, 6},
        three, four, fullhouse, sstraight, lstraight, yahtzee, chance}).
%% ====================================================================
%%                             Public API
%% ====================================================================
-export([serve_match/6]).
%% ====================================================================
%%                             Constants
%% ====================================================================
%% ====================================================================
%%                             Functions
%% ====================================================================

serve_match(Parent, P1, bye, Tid, Mid, _NumGame)->
	log("Player ~p has a bye in match ~p in tournament ~p.", [P1, Mid, Tid]),
	Parent ! {match_over, P1, bye, Mid, Tid};
serve_match(Parent, P1, P2, Tid, Mid, NumGame)->
	log("Beginning match between ~p and ~p.", [P1, P2]),
    case play_games(P1, P2, Tid, Mid, NumGame, 0, 0, 0, 0, false) of
        P1 ->
            Parent ! {match_over, P1, P2, Mid, Tid};
        P2 ->
            Parent ! {match_over, P2, P1, Mid, Tid};
        fault ->
            Parent ! {match_fault, P1, P2, Mid, Tid}
    end.

play_games(P1, P2, Tid, Mid, NumGame, P1Wins, P2Wins, Ties, Faults, Standard) ->
    Win = case P1Wins*2 >= NumGame of
        true -> {ok, P1};
        false ->
            case P2Wins*2 >= NumGame of
                true -> {ok, P2};
                false ->
                    case Ties*2 >= NumGame of
                        true ->
                            log("Tie match. Replaying with standard rules."),
                            play_games(P1, P2, Tid, Mid, NumGame, 0, 0, 0, 0, true);
                        false ->
                            case Faults*2 >= NumGame of
                                true ->
                                    log("Too many faults to finish match. I give up. Nobody wins."),
                                    {ok, fault};
                                false ->
                                    none
                            end
                    end
            end
    end,
    % collapse some cases
    case Win of
        {ok, X} -> 
            X;
        none ->
            case serve_game(P1, P2, Tid, make_ref(), Standard) of
                {ok, p1} ->
                    play_games(P1, P2, Tid, Mid, NumGame, P1Wins + 1, P2Wins, Ties, Faults, Standard);
                {ok, p2} ->
                    play_games(P1, P2, Tid, Mid, NumGame, P1Wins, P2Wins + 1, Ties, Faults, Standard);
                tie ->
                    play_games(P1, P2, Tid, Mid, NumGame, P1Wins, P2Wins, Ties + 1, Faults, Standard);
                cheaters ->
                    play_games(P1, P2, Tid, Mid, NumGame, P1Wins, P2Wins, Ties, Faults + 1, Standard)
            end
    end.

serve_game(P1, P2, Tid, Gid, IsStandard) ->
    turn(1, P1, P2, [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0], [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0], [], [], Tid, Gid, IsStandard).

get_score([T])-> T * 100;
get_score([Box|ScoreCard]) ->
	Box+get_score(ScoreCard).

% Game over, check winner
check_winner(P1ScoreCard, P2ScoreCard)->
	P1Score = get_score(P1ScoreCard),
	P2Score = get_score(P2ScoreCard),
	case P1Score == P2Score of
		true ->
			% Tie game, start game over
			log("Tie game, starting over."),
            tie;
		false ->
			case P1Score > P2Score of
				true ->
					% P1 is the winner
					log("P1 won!"),
                    {ok, p1};
				false ->
					% P2 is the winner
					log("P2 won!"),
                    {ok, p2}
			end
	end.


% 13 rounds for 1 game
turn(Round, P1, P2, P1ScoreCard, P2ScoreCard, P1Patterns, P2Patterns, Tid, Gid, IsStandard)->
	log("Round ~p between ~p and ~p.", [Round, P1, P2]),
    Dice1 = [crypto:rand_uniform(1, 7) || _X <- lists:seq(1, 15)],
    Dice2 = case IsStandard of
        true ->
            % Tie game, give different dice.
            [crypto:rand_uniform(1, 7) || _X <- lists:seq(1, 15)];
        false ->
            Dice1
    end,
	{P1Pattern, P1Dice} = run_round(P1, P1ScoreCard, P2ScoreCard, Tid, Gid, Dice1),
	{P2Pattern, P2Dice} = run_round(P2, P2ScoreCard, P1ScoreCard, Tid, Gid, Dice2),
    case {box_valid(P1ScoreCard, P1Pattern), box_valid(P2ScoreCard, P2Pattern)} of
        {true, false} ->
            {ok, p1};
        {false, true} ->
            {ok, p2};
        {false, false} ->
            cheaters;
        {true, true} ->
            % TODO: extra bonuses
            {P1Bonus, P1Box} = case score_round(P1Dice, element(P1Pattern, ?PATTERNS), P1ScoreCard) of
                {X, Y} -> {X, Y};
                Z -> {0, Z}
            end,
            {P2Bonus, P2Box} = case score_round(P2Dice, element(P2Pattern, ?PATTERNS), P2ScoreCard) of
                {X2, Y2} -> {X2, Y2};
                Z2 -> {0, Z2}
            end,
            NewP1SC = replace_nth(P1Pattern, P1Box, P1ScoreCard),
            NewP2SC = replace_nth(P2Pattern, P2Box, P2ScoreCard),	
            NewP1SC_ = replace_nth(14, lists:nth(14, P1ScoreCard) + P1Bonus, NewP1SC),
            NewP2SC_ = replace_nth(14, lists:nth(14, P2ScoreCard) + P2Bonus, NewP2SC),
            case Round of
                13 ->
                    check_winner(NewP1SC_, NewP2SC_);
                _ ->
                    turn(Round+1, P1, P2, NewP1SC_, NewP2SC_, P1Patterns++[{P1Box, P1Pattern}], P2Patterns++[{P2Box, P2Pattern}], Tid, Gid, IsStandard)
            end
    end.

box_valid(Boxes, Box) ->
    % Strict inequality bc last box is yahtzees
    case (Box >= 1) and (Box < length(Boxes)) of
        true -> (lists:nth(Box, Boxes) == -1);
        false -> false
    end.

replace_nth(1, Elem, [_H|T]) ->
    [Elem|T];
replace_nth(K, Elem, [H|T]) ->
    [H|replace_nth(K-1, Elem, T)].



% separate dice kept and the dice to be rerolled
prune_dice([], []) -> [];
prune_dice(Dice, [true | Keepers]) ->
	[hd(Dice)] ++ prune_dice(tl(Dice), Keepers);
prune_dice(Dice, [false | Keepers]) ->
	prune_dice(tl(Dice), Keepers).


run_round(Player, ScoreCard, OtherScoreCard, Tid, Gid, Dice) ->
    InitDice = lists:sublist(Dice, 5),
    RestDice = lists:sublist(Dice, 6, 10),
    {Name, Pid, _} = Player,
    assembly_phase(Name, Pid, ScoreCard, OtherScoreCard, Tid, Gid, InitDice, 1, RestDice).

assembly_phase(Name, Pid, ScoreCard, OppScoreCard, Tid, Gid, Dice, RollNum, Extra) ->
    Pid ! {play_request, self(), Name, {make_ref(), Tid, Gid, RollNum, Dice, ScoreCard, OppScoreCard}},
    receive
        {play_action, PlayerPid, Name, {_Ref, Tid, Gid, RollNum, Keepers, ScoreCardLine}}->
            case ScoreCardLine > 0 of
                true ->
                    log("Player ~p decided to stay on ~p.", [Name, Dice]),
                    {ScoreCardLine, Dice};
                false ->
                    case RollNum of
                        3 ->
                            log("Player ~p tried to keep playing after roll 3. Forfeiting.", [Name]),
                            {-1, [7,7,7,7,7]};
                        _ ->
                            DiceKept = prune_dice(Dice, Keepers),
                            case length(DiceKept) > 5 of
                                true ->
                                    log("Player ~p tried to keep too many dice, somehow", [Name]),
                                    {-1, [7,7,7,7,7]};
                                false ->
                                    SubsetExcl = Dice--DiceKept,	
                                    NewDice = lists:sort(DiceKept++lists:sublist(Extra, length(SubsetExcl))),
                                    NewExtra = lists:sublist(Extra, length(SubsetExcl) + 1, 15),
                                    assembly_phase(Name, PlayerPid, ScoreCard, OppScoreCard, Tid, Gid, NewDice, RollNum+1, NewExtra)
                            end
                    end
            end
    after 4000 ->
            log("Player ~p timed out", [Name]),
            cheat
    end.

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
score_round(Xs, {upper, X}, _ScoreCard) ->
    N = length([Y || Y <- Xs, Y == X]),
    log("Upper section ~p. ~p ~ps gives you ~p.", 
        [X, Xs, N, X * N]),
    X * N;
score_round([X, X, X, X, X], yahtzee, _ScoreCard ) -> 
	% Yahtzee
	log("Yahtzee! Mark it 50."),
    case lists:nth(12, _ScoreCard) > 0 of
        true -> log("Extra yahtzee bonus! 100 more points."),
            {1, 50};
        false ->
            50
    end;
	%% {50, [X, X, X, X, X]};
score_round([A, B, C, D, E], lstraight, _ScoreCard) when (E == D + 1) and (D == C + 1) and
		(C == B + 1) and (B == A + 1) ->
	% Large Straight
	log("Large straight! That's 40."),
    40;
	%% {40, [A, B, C, D, E]};
score_round([_A, B, C, D, E], sstraight, _ScoreCard) when (E == D + 1) and (D == C + 1) and
		(C == B + 1) ->
	log("Small straight! 30 isn't too small relatively."),
    30;
	%% {30, [A, B, C, D, E]};
score_round([A, B, C, D, _E], sstraight, _ScoreCard) when (D == C + 1) and (C == B + 1) and 
		(B == A + 1) ->
	log("Small straight! 30 isn't too small relatively."),
    30;
	%% {30, [A, B, C, D, E]};
score_round([A, A, A, B, B], fullhouse, _ScoreCard) when (A /= B)-> 
	% Full House 
	log("Full house. 25 shall be given."),
    25;
	%% {25, [A, A, A, B, B]};
score_round([B, B, A, A, A], fullhouse, _ScoreCard) when (A /= B) ->
	% Full House
	log("Full house. 25 shall be given."),
    25;
	%% {25, [B, B, A, A, A]};
score_round([X, X, X, X, Y], four, _ScoreCard) ->
	Sum = 4 * X + Y,
	log("Four of a kind. Sum is ~p.", [Sum]),	
    Sum;
	%% {Sum, [X, X, X, X, Y]};
score_round([Y, X, X, X, X], four, _ScoreCard) ->
	Sum = 4 * X + Y,
	log("Four of a kind. Sum is ~p.", [Sum]),	
    Sum;
	%% ({Sum, [Y, X, X, X, X]};
score_round([X, X, X, Y, Z], three, _ScoreCard) ->
	Sum = 3 * X + Y + Z,
	log("Three of a kind. Sum is ~p.", [Sum]),	
    Sum;
	%% ({Sum, [X, X, X, Y, Z]};
score_round([Y, X, X, X, Z], three, _ScoreCard) ->
	Sum = 3 * X + Y + Z,
	log("Three of a kind. Sum is ~p.", [Sum]),	
    Sum;
	%% ({Sum, [Y, X, X, X, Z]};
score_round([Y, Z, X, X, X], three, _ScoreCard) ->
	Sum = 3 * X + Y + Z,
	log("Three of a kind. Sum is ~p.", [Sum]),	
    Sum;
	%% ({Sum, [Y, Z, X, X, X]};
score_round([Y, Z, W, X, X], two, _ScoreCard) ->
	Sum = 2 * X + W + Y + Z,
	log("Two of a kind. Sum is ~p.", [Sum]),	
    Sum;
	%% ({Sum, [Y, Z, W, X, X]};
score_round([Y, Z, X, X, W], two, _ScoreCard) ->
	Sum = 2 * X + W + Y + Z,
	log("Two of a kind. Sum is ~p.", [Sum]),	
    Sum;
	%% ({Sum, [Y, Z, X, X, W]};
score_round([Y, X, X, W, Z], two, _ScoreCard) ->
	Sum = 2 * X + W + Y + Z,
	log("Two of a kind. Sum is ~p.", [Sum]),	
    Sum;
	%% ({Sum, [Y, X, X, W, Z]};
score_round([X, X, W, Y, Z], two, _ScoreCard) ->
	Sum = 2 * X + W + Y + Z,
	log("Two of a kind. Sum is ~p.", [Sum]),	
    Sum;
	%% ({Sum, [X, X, W, Y, Z]};
score_round([R1, R2, R3, R4, R5], chance, _ScoreCard)->
	Sum = R1 + R2 + R3 + R4 + R5,
	log("Chance. Sum is ~p.", [Sum]),
    Sum;
score_round(_, Pattern, _) ->
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
