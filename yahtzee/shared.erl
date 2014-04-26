-module(shared).
-export([timestamp/0, log/1, log/2]).

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

