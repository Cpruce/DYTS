%% CSCI182E - Distributed Systems
%% Harvey Mudd College
%% Distributed fault-tolerant yahtzee tournament system
%% @author Cory Pruce, Colin Stanfill
%% @doc _D157R18U73D_Y4H7533_
-module(yahtzee_manager).

-import(shared,[pred/1, pred_perms/2, timestamp/0, log/1, log/2, shuffle/1]).
-import(tournament_manager, [tournament_start/4]).
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

% Tournaments is a list of tuples: {Tid, TournamentPid, CallerPid, PlayerList}
%  The PlayerList here is a list of usernames.
% Same with PendingTournaments; it represents tournaments we've been asked to run but which have
%  not yet started.
% Players is a list of tuples:
%   {Username, Password, MaybeToken, Status, Record} - username, login token or null if logged out,
%                                       status is logged_in or logged_out
% TODO: add completed tournaments field
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
            case validate_password(Players, Username, Password) of
                ok ->
                    shared:log("Player ~p joined, giving them login token ~p");
                bad ->
                    shared:log("Player ~p tried to log  in with the wrong password!"),
                    manager_run(Tournaments, Players, PendingTournaments);
                error ->
                    shared:log("Adding player ~p.")
            end,
            Token = make_ref(),
            Pid ! {logged_in, self(), Username, Token},
            % We will only get here if the player info is valid
            case logged_in(Players, Username) of
                true ->
                    send_all_tm(Tournaments, {invalidate, Username}),
                    send_all_tm(Tournaments, {back, Username, Pid, Token});
                false ->
                    send_all_tm(Tournaments, {back, Username, Pid, Token})
            end,
            spawn(yahtzee_manager, monitor_player, [Username, Pid, self()]),
            % log_in will add the player if they don't exist, and update them if they do.
            Players_ = log_in(Players, Username, Password, Token),
            manager_run(Tournaments, Players_, PendingTournaments);
        {login, Pid, Username, {Username_, _}} ->
            shared:log("Pid ~p tried to log in as user ~p, but provided authentication for "
                "user ~p", [Pid, Username, Username_]),
            manager_run(Tournaments, Players, PendingTournaments);
    	{request_players, Pid, NumPlayers}->
            TournPlayers = get_n_players(Players, NumPlayers, []),
            log("Pid ~p asked for ~p players.~n", [Pid, NumPlayers]),
            Pid ! {add_players, TournPlayers},
            manager_run(Tournaments, Players, PendingTournaments);
	{tournament_begin, _Pid, Tid} ->
		log("Tournament ~p has begun.~n"),
		manager_run([{Tid, in_progress, undefined, []}]++Tournaments, Players, PendingTournaments--lists:keyfind(Tid, 1, PendingTournaments));
	{tournament_complete, Tid, Winner}->
		manager_run(lists:keyreplace(Tid, 1, Tournaments, {Tid, complete, Winner, []}), Players, PendingTournaments);
	{tournament_info, Pid, Tid}->
            case lists:keyfind(Tid, 1, Tournaments) of
		    false ->
			    case lists:keyfind(Tid, 1, PendingTournaments) of
				    false ->
					    log("Tournament requested by ~p was not found.~n", [Pid]),
					    manager_run(Tournaments, Players, PendingTournaments);
				    Tourn ->
					    log("Tournament request by ~p is ~p.~n", [Pid, Tourn]),
					    Pid ! {tournament_status, self(), Tourn},
			   		    manager_run(Tournaments, Players, PendingTournaments)
			   end;
		    Tourn ->
			log("Tournament request by ~p is ~p.~n", [Pid, Tourn]),
		 	Pid ! {tournament_status, self(), Tourn},
			manager_run(Tournaments, Players, PendingTournaments)
	    end;
        {_, missing, Username} ->
            shared:log("Player ~p has vanished.", [Username]),
            send_all_tm(Tournaments, {invalidate, Username}),
            Players_ = log_out(Players, Username),
			manager_run(Tournaments, Players_, PendingTournaments);
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

% get # player names
get_n_players([], X, _Acc) when X > 0 -> [];
get_n_players(_Players, 0, Acc) -> Acc;
get_n_players(Players, N, Acc) ->
	RandPlayer = lists:nth(crypto:rand_uniform(1, length(Players)), Players),
	case lists:member(RandPlayer, Players) of
		true ->
			get_n_players(Players, N, Acc);
		false ->
			get_n_players(Players, N-1, [RandPlayer]++Acc)
	end.

% validate_password(Players, Username, Password)
validate_password([], _, _) ->
    error;
validate_password([{Username, Password_, _, _, _}|_], Username, Password) ->
    case (Password_ == Password) of
        true -> ok;
        false -> bad
    end;
validate_password([_|T], Username, Password) ->
    validate_password(T, Username, Password).

logged_in([], _) -> error;
logged_in([{Username, _, _, logged_in, _}|_], Username) -> ok;
logged_in([{Username, _, _, logged_out, _}|_], Username) -> ok;
logged_in([_|T], Username) -> logged_in(T, Username).

log_in([], Username, Password, Token) ->
    [{Username, Password, Token, logged_in, []}];
log_in([{Username, Password, _, _, Record}|T] , Username, Password, Token) ->
    [{Username, Password, Token, logged_in, Record}|T];
log_in([H|T], Username, Password, Token) ->
    [H|log_in(T, Username, Password, Token)].

log_out([], _) -> [];
log_out([{Username, Password, _, logged_in, Record}|T], Username) ->
    [{Username, Password, none, logged_out, Record}|T];
log_out([H|T], Username) ->
    [H|log_out(T, Username)].


% Tournaments is a list of tuples: {Tid, TournamentPid, CallerPid, PlayerList}
send_all_tm([], _) -> ok;
send_all_tm([{_, Pid, _, _}|T], M) ->
    Pid ! M,
    send_all_tm(T, M).

% monitor players to make sure they are still in the game
monitor_player(Name, Pid, ParentPid) -> 
	erlang:monitor(yahtzee_player, Pid), %{RegName, Node}
	receive
	
		{'DOWN', _Ref, process, _Pid, normal} ->

			ParentPid ! {self(), normal, Name};

		{'DOWN', _Ref, process, _Pid, _Reason} ->

			ParentPid ! {self(), missing, Name}
	
	end.
 
