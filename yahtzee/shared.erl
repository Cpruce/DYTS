-module(shared).
-export([pred/1, pred_perms/2, timestamp/0, log/1, log/2, shuffle/1]).

% Generate a timestamp for the current microsecond.
-spec timestamp() -> io_lib:chars().
timestamp() ->
    Now = os:timestamp(),
    {_, _, Micros} = Now,
    {{Year, Month, Day}, {Hour, Minute, Second}} =
        calendar:now_to_universal_time(Now),
    io_lib:format("~2w/~2w/~2..0w ~2..0w:~2..0w:~2..0w.~6..0w",
                  [Month, Day, Year rem 100, Hour, Minute, Second, Micros]).

% Log a single string, timestamped
-spec log(string()) -> no_return().
log(X) -> log("~s", [X]).

% Log an io:format-style string-termlist pair, timestamped.
-spec log(string(), [term()]) -> no_return().
log(Format, Values) ->
    Data = io_lib:format(Format, Values),
    io:format("[~s]: ~s~n", [timestamp(), Data]).

-spec shuffle([term()]) -> [term()].
shuffle(List) ->
    shuffle_([], List).

% Fisher-Yates shuffle, referenced from http://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle
% Uses "inside-out" method. Not tested very thoroughly.
shuffle_(Lst, Src) ->
    case Src of
        [] -> Lst;
        [H|T] ->
            I = length(Lst),
            J = crypto:rand_uniform(0, I+1),
            case J of
                I ->
                    shuffle_(Lst ++ [H], T);
                _ ->
                    Lst_ = lists:sublist(Lst, J) ++ [H] ++ lists:nthtail(J+1, Lst) ++ [
                        lists:nth(J+1, Lst)],
                    shuffle_(Lst_, T)
            end
    end.

% Produce all permutations, code taken from http://erlang.org/pipermail/erlang-questions/2010-September/053287.html
pred(L = [H | _T]) when H > 1 ->
	    L;
pred(_L) ->
	    [].

do_pred_perms([], _Pred) -> 
	    [[]];
do_pred_perms(L, Pred)  -> 
	    [Pred([H|T]) || H <- L, T <- do_pred_perms(L--[H], Pred)].

pred_perms(List, Pred) ->
	    Res = do_pred_perms(List, Pred),
	        lists:filter(fun(E) -> E =/= [] end, Res).
