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
    log("Node is ~p, name is yahtzee_player, username is ~p",
        [node(), Name]),
    log("Name ~p Pwd ~p Tms ~p", [Name, Pwd, TMs]),
    player_begin(Name, Pwd, [list_to_atom(TM) || TM <- TMs], []),
    log("Bye-bye!"),
    halt().


player_begin(Name, _, [], []) ->
    log("No more tournaments to talk to."),
    log("Player ~p exiting.", [Name]);
% Exit here.

player_begin(Name, Pwd, [], Pids) ->
    log("All login requests sent. Waiting."),
    player_wait(Name, Pwd, Pids);

player_begin(Name, Pwd, TMs, Pids) ->
    TM = hd(TMs),
    Pid = spawn(fun() -> log_in(Name, Pwd, TM) end),
    erlang:monitor(process, Pid),
    player_begin(Name, Pwd, tl(TMs), [{Pid, TM}|Pids]).

player_wait(_Name, _Pwd, []) ->
    log("No children left to play. Going away.");

player_wait(Name, Pwd, Pids) ->
    receive
        {'DOWN', _, process, Pid, Info} ->
            shared:log("Process ~p went down: ~p.", [Pid, Info]),
            [{Pid, TM}] = [{Pid_, TM} || {Pid_, TM} <- Pids, Pid_ == Pid],
            Rest = [{Pid_, TM_} || {Pid_, TM_} <- Pids, Pid_ /= Pid],
            case Info of
                noproc ->
                    log("Process never existed! Not restarting"),
                    player_wait(Name, Pwd, Rest);
                noconnection ->
                    log("Process wasn't on our node, somehow!"),
                    player_wait(Name, Pwd, Rest);
                normal ->
                    log("Process exited normally. Probably means TM is down, not restarting."),
                    player_wait(Name, Pwd, Rest);
                _ ->
                    shared:log("Restarting connection to TM ~p.", [TM]),
                    player_begin(Name, Pwd, [TM], Rest)
            end
    end.
            
log_in(Name, Pwd, TM) ->
    {yahtzee_manager, TM} !  {login, self(), Name, {Name, Pwd}},
    receive
        {logged_in, Pid, Name, LoginTicket}->
            monitor(process, Pid),
            log("Logged in to TM ~p with ticket ~p", [TM, LoginTicket]),
            logged_in(Name, Pwd, TM, LoginTicket, []);
        {logged_in, Pid, Name_, LoginTicket}->
            monitor(process, Pid),
            log("Yahtzee manager things we're named ~p. Rolling with it ...", [Name_]),
            logged_in(Name_, Pwd, TM, LoginTicket, []);
        Other ->
            log("Bad message while waiting to log in: ~p", [Other]),
            log_in(Name, Pwd, TM)
    end.

% Messages sent to a logged-in player.
logged_in(Name, Pwd, TM, Ticket, Tournaments)->
    receive
        % Data is a single tournament identifier
        {start_tournament, Pid, Name, Tid} ->
            log("Received notification that tournament ~p is starting.~n", [Tid]),
            Pid ! {accept_tournament, self(), Name, {Tid, Ticket}},
            logged_in(Name, Pwd, TM, Ticket, Tournaments++[Tid]);
        {end_tournament, _, Name, Tid} ->
            log("Received notification that tournament ~p is ending.~n", [Tid]),
            logged_in(Name, Pwd, TM, Ticket, Tournaments--[Tid]);
        {play_request, Pid, Name, {Ref, Tid, Gid, RollNum, Dice, ScoreCard, OppScoreCard}}->
            log("Received roll of ~p on roll ~p in game ~p in tournament ~p. Player has ~p and opponent has ~p.~n", [Dice, RollNum, Gid, Tid, ScoreCard, OppScoreCard]),
            %% {Box, Pattern} = find_max(0, lists:map(fun(X)->scoring_process(X) end, pred_perms(Dice, fun shared:pred/1))),
            {Box, Line, Pattern} = scoring_process(lists:sort(Dice)),
            Keepers = choose_keepers(lists:sort(Dice)),
            case (Box >= 30) of 
                true ->
                    case lists:nth(Line, ScoreCard) of
                        -1 ->
                            ScoreCardLine = Line;
                        _ ->
                            case RollNum of
                                3 ->
                                    ScoreCardLine = lists:nth(1,
                                        [X || X <- lists:seq(1, 13), lists:nth(X, ScoreCard) == -1]);
                                _ ->
                                    ScoreCardLine = 0
                            end
                    end;
                false ->
                    case RollNum of
                        3 ->
                            case lists:nth(Line, ScoreCard) of
                                -1 ->
                                    ScoreCardLine = Line;
                                _ ->
                                    ScoreCardLine = lists:nth(1,
                                        [X || X <- lists:seq(1, 13), lists:nth(X, ScoreCard) == -1])
                            end;
                        _ ->
                            ScoreCardLine = 0
                    end
            end,
            Pid ! {play_action, self(), Name, {Ref, Tid, Gid, RollNum, Keepers, ScoreCardLine}},
            logged_in(Name, Pwd, TM, Ticket, Ticket);
        {Type, Pid, Name, Data} ->
            log("Received unknown message of type ~p from ~p containing ~p", [Type, Pid, Data]),
            logged_in(Name, Pwd, TM, Ticket, Tournaments);
        {Type, Pid, Name_, Data} ->
            log("Received message (~p) from ~p intended for ~p. Contains: ~p", [Type, Pid, Name_, Data]),
            logged_in(Name, Pwd, TM, Ticket, Tournaments);
        % We don't HAVE to assume TMs can go down, but it does happen...
		{'DOWN', _Ref, process, _Pid, _Reason} ->
            log("Tournament manager went down! Reason given = ~p. Giving up on life.", [_Reason]);
        Other -> 
            log("Received unknown message: ~p", [Other]),
            logged_in(Name, Pwd, TM, Ticket, Tournaments)
    end.

% find max score from each permutation of the dice
find_max(Max, [])-> Max;
find_max(Max, [Box|Perms])->
    case Max < Box of
        true ->
            find_max(Box, Perms);
        false ->
            find_max(Max, Perms)
    end.

% Returns a tuple of the Box filled in with the score 
% and the scorecard updated with the newest pattern.
scoring_process([X, X, X, X, X]) -> 
    % Yahtzee
    log("Yahtzee! Mark it 50.~n"),
    {50, 12, [X, X, X, X, X]};
scoring_process([A, B, C, D, E]) when (E == D + 1) and (D == C + 1) and (C == B + 1) and (B == A + 1) ->
    % Large Straight
    log("Large straight! That's 40.~n"),
    {40, 11, [A, B, C, D, E]};
scoring_process([A, B, C, D, E]) when (E == D + 1) and (D == C + 1) and (C == B + 1) ->
    log("Small straight! 30 isn't too small relatively.~n"),
    {30, 10, [A, B, C, D, E]};
scoring_process([A, B, C, D, E]) when (D == C + 1) and (C == B + 1) and (B == A + 1) ->
    log("Small straight! 30 isn't too small relatively.~n"),
    {30, 10, [A, B, C, D, E]};
scoring_process([A, A, A, B, B]) -> 
    % Full House 
    log("Full house. 25 shall be given.~n"),
    {25, 9, [A, A, A, B, B]};
scoring_process([B, B, A, A, A]) ->
    % Full House
    log("Full house. 25 shall be given.~n"),
    {25, 9, [B, B, A, A, A]};
scoring_process([X, X, X, X, Y]) ->
    Sum = 4 * X + Y,
    log("Four of a kind. Sum is ~p.~n", [Sum]),	
    {Sum, 8, [X, X, X, X, Y]};
scoring_process([Y, X, X, X, X]) ->
    Sum = 4 * X + Y,
    log("Four of a kind. Sum is ~p.~n", [Sum]),	
    {Sum, 8, [Y, X, X, X, X]};
scoring_process([X, X, X, Y, Z]) ->
    Sum = 3 * X + Y + Z,
    log("Three of a kind. Sum is ~p.~n", [Sum]),	
    {Sum, 7, [X, X, X, Y, Z]};
scoring_process([Y, X, X, X, Z]) ->
    Sum = 3 * X + Y + Z,
    log("Three of a kind. Sum is ~p.~n", [Sum]),	
    {Sum, 7, [Y, X, X, X, Z]};
scoring_process([Y, Z, X, X, X]) ->
    Sum = 3 * X + Y + Z,
    log("Three of a kind. Sum is ~p.~n", [Sum]),	
    {Sum, 7, [Y, Z, X, X, X]};
%% scoring_process([Y, Z, W, X, X]) ->
%%     Sum = 2 * X + W + Y + Z,
%%     log("Two of a kind. Sum is ~p.~n", [Sum]),	
%%     {Sum, 7, [Y, Z, W, X, X]};
%% scoring_process([Y, Z, X, X, W]) ->
%%     Sum = 2 * X + W + Y + Z,
%%     log("Two of a kind. Sum is ~p.~n", [Sum]),	
%%     {Sum, [Y, Z, X, X, W]};
%% scoring_process([Y, X, X, W, Z]) ->
%%     Sum = 2 * X + W + Y + Z,
%%     log("Two of a kind. Sum is ~p.~n", [Sum]),	
%%     {Sum, [Y, X, X, W, Z]};
%% scoring_process([X, X, W, Y, Z]) ->
%%     Sum = 2 * X + W + Y + Z,
%%     log("Two of a kind. Sum is ~p.~n", [Sum]),	
%%     {Sum, [X, X, W, Y, Z]};
scoring_process([R1, R2, R3, R4, R5])->
    Sum = R1 + R2 + R3 + R4 + R5,
    log("No pattern. Sum is ~p.~n", [Sum]),
    {Sum, 13, [R1, R2, R3, R4, R5]}.

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
        choose_keepers([_A, B, C, D, E]) when (E == D + 1) and (D == C + 1) and
        (C == B + 1) ->
            % Small Straight, might as well go for the Large Straight
            log("Small straight! Keep everything except the first entry.~n"),
            [false, true, true, true, true];
        choose_keepers([A, B, C, D, _E]) when (D == C + 1) and (C == B + 1) and 
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
        choose_keepers([X, X, X, X, _Y]) ->
            % Four of a Kind, might as well go for the Yahtzee
            log("Four of a kind. Keep everything except the last entry.~n"),	
            [true, true, true, true, false];
        choose_keepers([_Y, X, X, X, X]) ->
            % Four of a Kind, might as well go for the Yahtzee
            log("Four of a kind. Keep everything except the first entry.~n"),	
            [false, true, true, true, true];
        choose_keepers([X, X, X, _Y, _Z]) ->
            % Three of a kind, shoot for four of a kind or Yahtzee
            log("Three of a kind. Keep everything except the last two entries.~n"),	
            [true, true, true, false, false];
        choose_keepers([_Y, X, X, X, _Z]) ->
            % Three of a kind, shoot for four of a kind or Yahtzee
            log("Three of a kind. Keep everything except the first entry and the last entry.~n"),
            [false, true, true, true, false];
        choose_keepers([_Y, _Z, X, X, X]) ->
            % Three of a kind, shoot for four of a kind or Yahtzee
            log("Three of a kind. Keep everything except the first two entries.~n"),
            [false, false, true, true, true];
        choose_keepers([_Y, _Z, _W, X, X]) ->
            % Two of a kind, shoot for more
            log("Two of a kind. Keep the pair.~n"),
            [false, false, false, true, true];
        choose_keepers([_Y, _Z, X, X, _W]) ->
            % Two of a kind, shoot for more
            log("Two of a kind. Keep the pair.~n"),
            [false, false, true, true, false];
        choose_keepers([_Y, X, X, _W, _Z]) ->
            % Two of a kind, shoot for more
            log("Two of a kind. Keep the pair.~n"),
            [false, true, true, false, false];
        choose_keepers([X, X, _W, _Y, _Z]) ->
            % Two of a kind, shoot for more
            log("Two of a kind. Keep the pair.~n"),
            [true, true, false, false, false];
        choose_keepers([_R1, _R2, _R3, _R4, _R5])->
            % No pattern
            log("No pattern, get new set of 5"),
            [false, false, false, false, false].
        %case (R2 == R1 + 1 and R3 == R2 + 1) of
        %	or (R2 == R1 + 1 and R4 == R3 + 1) 		or (R2 == R1 + 1 and R.

