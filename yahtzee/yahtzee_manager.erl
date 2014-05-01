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
    manager_run([], [], []).

% Tournaments is a list of tuples: {Tid, TournamentPid, PlayerList}
%  The PlayerList here is a list of usernames
% Players is a list of tuples:
%   {Username, MaybeToken, Status, Record} - username, login token or null if logged out,
%                                       status is logged_in or logged_out
manager_run(Tournaments, Players, PendingTournaments)->
    receive
        % Data is a tuple {num-players, games-per-match}
        {request_tournament, Pid, {NumPlayers, Gpm}}->
            Tid = make_ref(),
            NewTourn = spawn(fun() ->
                        tournament_manager:tournament_start(self(), Tid, NumPlayers, Gpm)
                end),
            manager_run(Tournaments, Players, [{Tid, Pid, NewTourn}|PendingTournaments]);
        {login, Pid, Username, {Username, Password}} ->
            % TODO: existing players
            Password,
            shared:log("Player ~p joined, giving them login token ~p"),
            Token = make_ref(),
            Pid ! {logged_in, self(), Pid, Token},
            Players_ = {Username, Token, logged_in, []},
            manager_run(Tournaments, Players_, PendingTournaments);
        {login, Pid, Username, {Username_, _}} ->
            shared:log("Pid ~p tried to log in as user ~p, but provided authentication for "
                "user ~p", [Pid, Username, Username_]),
            manager_run(Tournaments, Players, PendingTournaments);
        {tournament_info, Pid, _}->
            % FIXME
            shared:log("Tournament info requested by pid ~p", [Pid]),
            Pid ! {tournament_status, self(), {0, complete, [], error}},
            manager_run(Tournaments, Players, PendingTournaments);
        {Other, Pid, Username, Data} ->
            shared:log("Player ~p @ ~p sent us garbage (type = ~p): ~p",
                [Username, Pid, Other, Data]),
            manager_run(Tournaments, Players, PendingTournaments);
        {Other, Pid, Data} ->
            shared:log("Unknown message of type ~p sent to us by pid ~p", [Other, Pid]),
            shared:log("Contains data: ~p", [Data]),
            manager_run(Tournaments, Players, PendingTournaments);
        Other ->
            shared:log("Unparseable message: ~p", [Other]),
            manager_run(Tournaments, Players, PendingTournaments)
    end.
