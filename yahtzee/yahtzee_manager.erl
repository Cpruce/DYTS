%% CSCI182E - Distributed Systems
%% Harvey Mudd College
%% Distributed fault-tolerant yahtzee tournament system
%% @author Cory Pruce, Colin Stanfill
%% @doc _D157R18U73D_Y4H7533_
-module(yahtzee_manager).

-import(shared).
-import(tournament_manager).
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
    % The first and only parameter is the name the yahtzee_manager 
    % registers itself with. This should be all lowercase with no 
    % @ signs, as on previous assignments, and should also be used 
    % as the node short name to start the network kernel.
    Name = hd(Params),
    EName = list_to_atom(Name),
    % IMPORTANT: Start the empd daemon!
    os:cmd("epmd -daemon"),
    % format microseconds of timestamp to get an
    % effectively-unique node name
    net_kernel:start([EName, shortnames]),
    shared:log("name is ~p", [Name]),
    register(yahtzee_manager, self()),
    shared:log("node() is ~p~n", [node()]),
    % Start with no active tournament managers, and zero players.
    manager_run([], []).

% Start tournament of a given size upon request from the environment.
% Tournaments is a list of tuples: {Tid, TournamentPid, PlayerList}
%  The PlayerList here is a list of usernames
% Players is a list of tuples: {Username, Ticket}
manager_run(Tournaments, Players)->
    receive
        % Data is a tuple {num-players, games-per-match}
        {request_tournament, Pid, {NumPlayers, Gpm}}->
            % TODO: Select players, note what tournaments they're in
            Tid = make_ref(),
            PlayersTapped = case length(Players) < NumPlayers of
                true ->
                    shared:log("Tournament requested with ~p players, but only ~p players"
                        "present.", [NumPlayers, length(Players)]),
                    Players;
                false ->
                    Players_ = shared:shuffle(Players),
                    lists:sublist(Players_, NumPlayers)
            end,
            NewTourn = spawn(fun() ->
                        tournament_manager:tournament_start(PlayersTapped, self(), Tid)
                end),
            Pid ! {tournament_started, self(), {Tid, PlayersTapped, ok}},
            Tournaments_ = [{Tid, NewTourn, PlayersTapped} | Tournaments],
            manager_run(Tournaments_, Players);
        {tournament_info, Pid, Data}->
            shared:log("Tournament info requested by pid ~p", [Pid]),
            Pid ! {tournament_status, self(), {0, complete, [], error}},
            manager_run(Tournaments, Players);
        {Other, Pid, Data} ->
            shared:log("Unknown message of type ~p sent to us by pid ~p", [Other, Pid]),
            shared:log("Contains data: ~p", [Data]),
            manager_run(Tournaments, Players);
        Other ->
            shared:log("Unparseable message: ~p", [Other]),
            manager_run(Tournaments, Players)
    end.
