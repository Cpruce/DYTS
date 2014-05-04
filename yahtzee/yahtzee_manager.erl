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
    log("name is ~p", [Name]),
    register(yahtzee_manager, self()),
    log("node() is ~p", [node()]),
    % Start with no active tournament managers, and zero players.
    manager_run([], [], []).

% Tournaments is a list of tuples: {Tid, TournamentPid, CallerPid, PlayerList}
%  The PlayerList here is a list of usernames.
% Same with PendingTournaments; it represents tournaments we've been asked to run but which have
%  not yet started.
% Players is a list of tuples:
%   {Username, Password, MaybeToken, Pid, Record} - username, login token or null if logged out,
%                                       Pid is a pid or logged_out
% TODO: add completed tournaments field
manager_run(Tournaments, Players, PendingTournaments)->
    receive
        % Data is a tuple {num-players, games-per-match}
        {request_tournament, Pid, {NumPlayers, Gpm}}->
            Tid = make_ref(),
            Parent = self(),
            NewTourn = spawn(fun() ->
                        tournament_manager:tournament_start(Parent, Tid, NumPlayers, Gpm)
                end),
            receive
                {tournament_players, TPid, TPlayers}->
                    ;
                Other ->
                    log("Error in tournament players message")
            end, 
            manager_run(Tournaments, Players, [{Tid, Pid, NewTourn}|PendingTournaments]);
        {login, Pid, Username, {Username, Password}} ->
            case validate_password(Players, Username, Password) of
                ok ->
                    shared:log("Player ~p joined, giving them login token ~p");
                bad ->
                    shared:log("Player ~p tried to log in with the wrong password!"),
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
                    send_all_tm(Tournaments, {back, Username, Pid, Token});
                error ->
                    log("New player; don't have to tell anyone they're back.")
            end,
            ParentPid = self(),
            spawn(fun() -> monitor_player(Username, Pid, ParentPid) end),
            % log_in will add the player if they don't exist, and update them if they do.
            Players_ = log_in(Players, Username, Password, Token, Pid),
            log("players: ~p", [Players_]),
            manager_run(Tournaments, Players_, PendingTournaments);
        {login, Pid, Username, {Username_, _}} ->
            shared:log("Pid ~p tried to log in as user ~p, but provided authentication for "
                "user ~p", [Pid, Username, Username_]),
            manager_run(Tournaments, Players, PendingTournaments);
    	{request_players, Pid, NumPlayers}->
            EligPlayers = [Player || Player <- Players, element(4, Player) /= logged_out],
            log("Pid ~p asked for ~p players with ~p eligible players.", [Pid,
                    NumPlayers, length(EligPlayers)]),
            TournPlayers = get_n_players(EligPlayers, NumPlayers, []),
            TournPlayers_ = [{Player, PPid, Token} || {Player, _, Token, PPid, _} <- TournPlayers],
            Pid ! {add_players, TournPlayers_},
            log("Sent: ~p", [TournPlayers_]),
            manager_run(Tournaments, Players, PendingTournaments);
        {tournament_begin, _Pid, Tid, TPlayers} ->
            log("Tournament ~p has begun.~n"),
            Pending_ = [{Tid_, Requester_, Pid_} || {Tid_, Requester_, Pid_} <- PendingTournaments, Tid_ /= Tid],
            [Requester] = [Requester_ || {Tid_, Requester_, _Pid_} <- PendingTournaments, Tid_ == Tid],
            Requester ! {tournament_started, self(), {Tid, TPlayers, ok}},
            NPlayers = new_tourn(Players, TPlayers),
            manager_run([{Tid, in_progress, undefined, []}]++Tournaments, NPlayers, Pending_);
        {tournament_complete, Tid, Winner}->
            {WinnerR, PwdW, TokenW, PidW, {MwsW, MlsW, TpW, TwW}} =
            lists:keyfind(Winner, 1, Players),
            NPlayers = lists:keyreplace(Winner, 1, Players, {WinnerR,
                    PwdW, TokenW, PidW, {MwsW, MlsW, TpW, TwW+1}}),
            manager_run(lists:keyreplace(Tid, 1, Tournaments, {Tid, complete,
                        Winner, []}), NPlayers, PendingTournaments);
        {user_info, Pid, Uname} ->
            log("Received user_info request of ~p from ~p", [Uname, Pid]),
            Pid ! {user_status, self(), lists:keyfind(Uname, 1, Tournaments)},
            manager_run(Tournaments, Players, PendingTournaments); 
        {match_results, Pid, Winner, Loser} ->
            {WinnerR, PwdW, TokenW, PidW, {MwsW, MlsW, TpW, TwW}} =
            lists:keyfind(Winner, 1, Players),
            NPlayers = lists:keyreplace(Winner, 1, Players, {WinnerR,
                    PwdW, TokenW, PidW, {MwsW+1, MlsW, TpW, TwW}}),
            {LoserR, PwdL, TokenL, PidL, {MwsL, MlsL, TpL, TwL}} =
            lists:keyfind(Loser, 1, NPlayers),
            NNPlayers = lists:keyreplace(Loser, 1, NPlayers, {LoserR, PwdL,
                    TokenL, PidL, {MwsL, MlsL+1, TpL, TwL}}),    
            manager_run(Tournaments, NNPlayers, PendingTournaments);
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
        {_Pid, missing, Username} ->
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

new_tourn(Players, [])->
    Players;
new_tourn(Players, [{A,B,C,D,{Mws, Mls, Tp, Tws}}|TPlayers])->
    new_tourn(lists:keyreplace(A, 1, Players, {A,B,C,D,{Mws,Mls,Tp+1,Tws}}),
        TPlayers).

% get # player names, using shuffle this time 
get_n_players2(Players, N) ->
    lists:sublist(shuffle(Players), N).

get_n_players([], X, _Acc) when X > 0 -> [];
get_n_players(_Players, 0, Acc) -> Acc;
get_n_players(Players, N, Acc) ->
	RandPlayer = lists:nth(crypto:rand_uniform(1, length(Players)+1), Players),
	case lists:member(RandPlayer, Acc) of
		true ->
			get_n_players(Players, N-1, Acc);
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
logged_in([{Username, _, _, _, _}|_], Username) -> true;
logged_in([{Username, _, _, logged_out, _}|_], Username) -> false;
logged_in([_|T], Username) -> logged_in(T, Username).

log_in([], Username, Password, Token, Pid) ->
    [{Username, Password, Token, Pid, []}];
log_in([{Username, Password, _, _, Record}|T] , Username, Password, Token, Pid) ->
    [{Username, Password, Token, Pid, Record}|T];
log_in([H|T], Username, Password, Token, Pid) ->
    [H|log_in(T, Username, Password, Token, Pid)].

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
	erlang:monitor(process, Pid), %{RegName, Node}
    log("Monitoring player ~p @ ~p for parent ~p", [Name, Pid, ParentPid]), 
	receive
	
		{'DOWN', _Ref, process, _Pid, normal} ->

            log("Normal"),
			ParentPid ! {self(), normal, Name};

		{'DOWN', _Ref, process, _Pid, _Reason} ->

            log("User ~p down!", [Name]),
			ParentPid ! {self(), missing, Name}
	
	end.
 
