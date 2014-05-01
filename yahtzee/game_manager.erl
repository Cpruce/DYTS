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

serve_game(P1, P1Pid, P2, P2Pid, Tid, Gid)->
	log("Beginning game between ~p and ~p.~n", [P1, P2]),
	% spawns two processes to monitor P1 and P2
	spawn(fun() -> monitor_player(P1, P1Pid, self()) end),
	spawn(fun() -> monitor_player(P2, P2Pid, self()) end),		
	turn(0, P1, P2, [], [], Tid, Gid).

get_score([])-> 0;
get_score([{Box, _Pattern}|ScoreCard] ->
	Box+get_score(ScoreCard).

% Game over, check winner
check_winner(P1, P2, P1ScoreCard, P2ScoreCard, Tid, Gid)->
	P1Score = get_score(P1ScoreCard),
	P2Score = get_score(P2ScoreCard),
	case P1Score == P2Score of
		true ->
			% Tie game, start game over
			log("Tie game, starting over.~n"),
			turn(0, P1, P2, [], [], Tid, Gid);
		false ->
			case P1Score > P2Score of
				true ->
					% P1 is the winner
					log("P1 won!~n"),

				false ->
					% P2 is the winner
					log("P2 won!~n"),
			end
	end.


% 13 rounds for 1 game
turn(13, P1, P2, P1ScoreCard, P2ScoreCard, Tid, Gid)->
	log("Last turn between ~p and ~p.~n", [P1, P2]),
	{P1Box, P1Pattern} = assembly_phase(P1, P1ScoreCard, P2ScoreCard, [], Tid, Gid, 13, 0),
	{P2Box, P2Pattern} = assembly_phase(P2, P2ScoreCard, P1ScoreCard, [], Tid, Gid, 13, 0),
	check_winner(P1, P2, P1ScoreCard++[{P1Box, P1Pattern}], P2ScoreCard,++[{P2Box, P2Pattern}], Tid, Gid).
turn(RollNum, P1, P2, P1ScoreCard, P2ScoreCard, Tid, Gid)->
	log("Turn ~p between ~p and ~p.~n", [RollNum, P1, P2]),
	{P1Box, P1Pattern} = assembly_phase(P1, P1ScoreCard, P2ScoreCard, [], Tid, Gid, RollNum, 0),
	{P2Box, P2Pattern} = assembly_phase(P2, P2ScoreCard, P1ScoreCard, [], Tid, Gid, RollNum, 0),
 	turn(RollNum+1, P1, P2, P1ScoreCard++[{P1Box, P1Pattern}], P2ScoreCard++[P2Box, P2Pattern], Tid, Gid).

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
	% round over, give both players both scorecards
	Player ! {turn_over, self(), {make_ref(), Dice, Tid, Gid, ScoreCard, OppScoreCard}},
	receive
		{player_ready, P1Pid, {Ref, Tid, Gid}}-> 
			log("Player 1 has received the scorecards.~n"),
			ok;
		_ -> ok
	end.

% monitor players to make sure they are still in the game
monitor_player(Name, Pid, ParentPid) -> 
	erlang:monitor(yahtzee_player, Pid), %{RegName, Node}
	receive
	
		{'DOWN', _Ref, process, _Pid, normal} ->

			ParentPid ! {self(), normal, Name};

		{'DOWN', _Ref, process, _Pid, _Reason} ->

			ParentPid ! {self(), missing, Name}
	
	end.
 
