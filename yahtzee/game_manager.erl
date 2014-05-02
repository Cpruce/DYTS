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
	turn(Parent, 0, P1, P2, [], [], Tid, Mid).

get_score([])-> 0;
get_score([{Box, _Pattern}|ScoreCard]) ->
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
	{P1Box, P1Pattern} = assembly_phase(P1, P1ScoreCard, P2ScoreCard, [], Tid, Mid, 13, 0),
	{P2Box, P2Pattern} = assembly_phase(P2, P2ScoreCard, P1ScoreCard, [], Tid, Mid, 13, 0),
	check_winner(Parent, P1, P2, P1ScoreCard++[{P1Box, P1Pattern}], P2ScoreCard++[{P2Box, P2Pattern}], Tid, Mid);
turn(Parent, RollNum, P1, P2, P1ScoreCard, P2ScoreCard, Tid, Mid)->
	log("Turn ~p between ~p and ~p.~n", [RollNum, P1, P2]),
	{P1Box, P1Pattern} = assembly_phase(P1, P1ScoreCard, P2ScoreCard, [], Tid, Mid, RollNum, 0),
	{P2Box, P2Pattern} = assembly_phase(P2, P2ScoreCard, P1ScoreCard, [], Tid, Mid, RollNum, 0),
 	turn(Parent, RollNum+1, P1, P2, P1ScoreCard++[{P1Box, P1Pattern}], P2ScoreCard++[P2Box, P2Pattern], Tid, Mid).

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
			scoring_process(Player, ScoreCard, OppScoreCard, NewDice, Tid, Mid, RollNum);
		_ -> 
			log("Unexpected message at roll")
	end;
assembly_phase(Player, ScoreCard, OppScoreCard, Dice, Tid, Mid, RollNum, 5) ->
        % 1st modification attempt
	SortedDice = lists:sort(Dice),	
	Player ! {play_request, self(), {make_ref(), Tid, Mid, RollNum, SortedDice, ScoreCard, OppScoreCard}},
	receive
		{play_action, PlayerPid, {Ref, Tid, Mid, RollNum, Keepers, ScoreCardLine}}->
		        DiceKept = prune_dice(SortedDice, Keepers),
			SubsetReroll = SortedDice--DiceKept,	
			NewDice = lists:sort(DiceKept++reroll(SubsetReroll)),
			assembly_phase(Player, ScoreCard, OppScoreCard, NewDice, Tid, Mid, RollNum, 6);
		_ -> 
			log("Unexpected message at roll")
	end;
assembly_phase(Player, ScoreCard, OppScoreCard, Dice, Tid, Mid, RollNum, DieRoll)->
	% Generate random roll 1 <= N < 7
	Rnd = crypto:rand_uniform(1, 7),
	assembly_phase(Player, ScoreCard, OppScoreCard, [Rnd]++Dice, Tid, Mid, RollNum, DieRoll+1).

% reroll attempts
reroll([]) -> [];
reroll(Subset) ->
	[crypto:rand_uniform(1, 7)] ++ reroll(tl(Subset)).	

% scoring phase, return the updated scorecard
scoring_phase(Player, ScoreCard, OppScoreCard, Dice, Tid, Mid, RollNum)->
	log("Beginning scoring phase for ~p in round ~p in game ~p in tournament ~p.~n", [Player, RollNum, Mid, Tid]),
	% round over, give both players both scorecards
	Player ! {turn_over, self(), {make_ref(), Dice, Tid, Mid, ScoreCard, OppScoreCard}},
	receive
		{player_ready, P1Pid, {Ref, Tid, Mid}}-> 
			log("Player 1 has received the scorecards.~n"),
			ok;
		_ -> ok
	end.


