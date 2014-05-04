Assignment 6: Yahtzee tournament
===========================
HMC CS182E Spring 2014: Distributed Systems

Cory Pruce
Colin Stanfill

Running
---------------------------
erlc shared.erl
erlc yahtzee_manager.erl
erlc yahtzee_player.erl
erlc game_manager.erl

1st Computer:
-----------------
erl -noshell -run yahtzee_manager main {shortname}

2nd-nth Computer(s):
-----------------
erl -noshell -run yahtzee_player main {shortnameN} {pwdN} {yahtzee-managers}

3rd Computer:
-----------------
Send tests such as a {request_tournament, self(), {NumPlayers,
Games-Per-Match}} request to start a tournament. message.erl is a test program
built specifically for request_tournamnet messages. To run:

erlc message.erl
erl -noshell -run message main request_tournament 2 3 {yahtzee-manager}


Comments
---------------------------
This implementation splits the tasks between yahtzee-managers and
tournament-mangers in a way similar to many-to-one NAT in which the
yahtzee-manager is the outside proxy for its tournament-managers, all of which
run individual tournaments. From that level, the game managers are spawned for
each match.


