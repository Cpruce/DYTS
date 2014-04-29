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

% Messages sent to a logged-in player.
logged_in(Name, Pwd, TMs, Tournaments, LoginTicket)->
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
			logged_in(Name, Pwd, TMs, Tournaments, LoginTicket)
		_ -> 
			log("Received  unknown message")
	end.

%% NOT FINISHED %% (Also can be used for when the scorecard is updated

% Returns list of booleans (true or false atoms)
% Assumes that a sorted list was supplied
choose_keepers([X, X, X, X, X]) -> 
	% Yahtzee
	[true, true, true, true, true];
choose_keepers([A, B, C, D, E]) when E == D + 1 and D == C + 1 and
		C == B + 1 and B == A + 1 ->
	% Large Straight
	[true, true, true, true, true];
choose_keepers([A, B, C, D, E]) when E == D + 1 and D == C + 1 and
		C == B + 1 ->
	% Small Straight, might as well go for the Large Straight
	[false, true, true, true, true];
choose_keepers([A, B, C, D, E]) when D == C + 1 and C == B + 1 and 
		B == A + 1 ->
	% Small Straight, might as well go for the Large Straight
	[true, true, true, true, false];	
choose_keepers([A, A, A, B, B]) -> 
	% Full House 
	[true, true, true, true, true];
choose_keepers([B, B, A, A, A]) ->
	% Full House
	[true, true, true, true, true];
	
choose_keepers([R1, R2, R3, R4, R5])->.

% NEED TO DO, return an integer representing a line on the scorecard in
% which to score the dice.
choose_line(ScoreCard, Dice, Keepers)->ok.
