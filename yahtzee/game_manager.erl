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
	P1Roll = assembly_phase([], 0),
	P2Roll = assembly_phase([], 0),
	P1 ! {play_request, self(), {make_ref(), Tid, Gid, 13, P1Roll, P1ScoreCard, P2ScoreCard}},
	receive
		{play_action, P1Pid, {Ref, Tid, Gid, 13, P1Keepers, P1ScoreCardLine}}-> 
			ok;
		_ -> ok
	end,
	P2 ! {play_request, self(), {make_ref(), Tid, Gid, 13, P2Roll, P2ScoreCard, P1ScoreCard}},
	receive
		{play_action, P2Pid, {Ref, Tid, Gid, 13, P2Keepers, P2ScoreCardLine}}-> 
			ok;
		_ -> ok
	end;
turn(RollNum, P1, P2, P1ScoreCard, P2ScoreCard, Tid, Gid)->
	log("Turn ~p between ~p and ~p.~n", [RollNum, P1, P2]),
	P1Roll = assembly_phase(P1, P1ScoreCard, P2ScoreCard, [], Tid, Gid, RollNum, 0),
	P2Roll = assembly_phase(P2, P2ScoreCard, P1ScoreCard, [], Tid, Gid, RollNum, 0).

% separate dice kept and the dice to be rerolled
prune_dice([], []) -> [];
prune_dice(Dice, [true | Keepers]) ->
	[hd(Dice)]

% emulate random roll of five dice and then two modification attempts
assembly_phase(Player, ScoreCard, OppScoreCard, Dice, Tid, Gid, RollNum, 5) ->
        SortedDice = lists:sort(Dice),	
	Player ! {play_request, self(), {make_ref(), Tid, Gid, RollNum, SortedDice, ScoreCard, OppScoreCard}},
	receive
		{play_action, PlayerPid, {Ref, Tid, Gid, RollNum, Keepers, ScoreCardLine}}->
		        DiceKept = prune_dice(SortedDice, Keepers),
			SubsetReroll = SortedDice--DiceKept,	
			assembly_phase(Player, ScoreCard, OppScoreCard, DiceKept, SubsetReroll, Tid, Gid, RollNum, 6);
		_ -> 
			log("Unexpected message at roll")
	end;
assembly_phase(Player, ScoreCard, OppScoreCard, Dice, Tid, Gid, Round)->
	% Generate random roll 1 <= N < 7
	Rnd = crypto:rand_uniform(1, 7),
	assembly_phase(Player, [Rnd]++Dice, Round+1).

