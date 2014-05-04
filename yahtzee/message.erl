-module(message).
-import(shared, [pred/1, pred_perms/2, timestamp/0, log/1, log/2, shuffle/1]).

-export([main/1]).

main(Params)->
    os:cmd("epmd -daemon"),
    % format microseconds of timestamp to get an
    % effectively-unique node name
    net_kernel:start([test, shortnames]),
    log("name is ~p", [test]),
    register(test, self()),
    log("node() is ~p", [node()]),
    AtomStr = hd(Params),
    NumPlayers = hd(tl(Params)),
    Gpm = hd(tl(tl(Params))),
    RegName = hd(tl(tl(tl(Params)))),
    SendName = list_to_atom(RegName), 
    Message = list_to_atom(AtomStr), 
    {yahtzee_manager, SendName} ! {Message,
        self(),{list_to_integer(NumPlayers), list_to_integer(Gpm)}}.
