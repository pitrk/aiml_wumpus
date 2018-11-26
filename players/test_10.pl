/* -*- mode: Prolog; comment-column: 48 -*- */
% Based on simple_player7 by Witold Paluszynski

act(Action, Knowledge) :-
	not(gameStarted),
	assert(gameStarted),

	worldSize(X,Y),
	assert(myWorldSize(X,Y)),
	assert(myPosition(1, 1, east)),
	assert(myTrail([])),
	assert(haveGold(0)),
	act(Action, Knowledge).

act(Action, Knowledge) :- go_back_step(Action, Knowledge).
act(Action, Knowledge) :- exit_if_home(Action, Knowledge).
act(Action, Knowledge) :- pick_up_gold(Action, Knowledge).
act(Action, Knowledge) :- turn_if_wall(Action, Knowledge).
act(Action, Knowledge) :- else_move_on(Action, Knowledge).

exit_if_home(Action, Knowledge) :-
	haveGold(NGolds), NGolds > 0,
	myPosition(1, 1, Orient),
	Action = exit,
	Knowledge = [].

go_back_step(Action, Knowledge) :-
	haveGold(NGolds), NGolds > 0,
	myWorldSize(Max_X, Max_Y),
	myTrail(Trail),
	Trail = [ [grab,X,Y,Orient] | Trail_Tail ],
	New_Trail = [ [turnRight,X,Y,Orient] | Trail_Tail ],
	Action = turnLeft,
	Knowledge = [gameStarted,
	             haveGold(NGolds),
		     myWorldSize(Max_X, Max_Y),
		     myPosition(X, Y, Orient),
		     myTrail(New_Trail)].

go_back_step(Action, Knowledge) :-
	haveGold(NGolds), NGolds > 0,
	myWorldSize(Max_X, Max_Y),
	myTrail([ [Action,X,Y,Orient] | Trail_Tail ]),
	Action = moveForward,
	Knowledge = [gameStarted,
	             haveGold(NGolds),
		     myWorldSize(Max_X, Max_Y),
		     myPosition(X, Y, Orient),
		     myTrail(Trail_Tail)].

go_back_step(Action, Knowledge) :- go_back_turn(Action, Knowledge).

go_back_turn(Action, Knowledge) :-
	haveGold(NGolds), NGolds > 0,
	myWorldSize(Max_X, Max_Y),
	myTrail([ [OldAct,X,Y,Orient] | Trail_Tail ]),
	((OldAct=turnLeft,Action=turnRight);(OldAct=turnRight,Action=turnLeft)),
	Knowledge = [gameStarted,
	             haveGold(NGolds),
		     myWorldSize(Max_X, Max_Y),
		     myPosition(X, Y, Orient),
		     myTrail(Trail_Tail)].

pick_up_gold(Action, Knowledge) :-
	glitter,
	Action = grab,
	haveGold(NGolds),
	NewNGolds is NGolds + 1,
	myWorldSize(Max_X, Max_Y),
	myPosition(X, Y, Orient),
	myTrail(Trail),
	New_Trail = [ [Action,X,Y,Orient] | Trail ],
	Knowledge = [gameStarted,
	             haveGold(NewNGolds),
		     myWorldSize(Max_X, Max_Y),
		     myPosition(X, Y, Orient),
		     myTrail(New_Trail)].

turn_if_wall(Action, Knowledge) :-
	myPosition(X, Y, Orient),
	myWorldSize(Max_X,Max_Y),
	againstWall(X, Y, Orient, Max_X, Max_Y),
	Action = turnLeft,
	left(Orient, NewOrient),
	haveGold(NGolds),
	myTrail(Trail),
	New_Trail = [ [Action,X,Y,Orient] | Trail ],
	Knowledge = [gameStarted,
		     haveGold(NGolds),
	             myWorldSize(Max_X, Max_Y),
		     myPosition(X, Y, NewOrient),
		     myTrail(New_Trail)].

againstWall(X, Y, Orient, Max_X, Max_Y) :- X = Max_X, Y = 1,     Orient = east.
againstWall(X, Y, Orient, Max_X, Max_Y) :- X = Max_X, Y = Max_Y, Orient = north.
againstWall(X, Y, Orient, Max_X, Max_Y) :- X = 1,     Y = Max_Y, Orient = west.
againstWall(X, Y, Orient, Max_X, Max_Y) :- X = 1,     Y = 1,     Orient = south.

left(east, north).
left(north, west).
left(west, south).
left(south, east).

right(north, east).
right(east, south).
right(south, west).
right(west, north).

else_move_on(Action, Knowledge) :-
	Action = moveForward,
	haveGold(NGolds),
	myWorldSize(Max_X,Max_Y),
	myPosition(X, Y, Orient),
	forwardStep(X, Y, Orient, New_X, New_Y),
	myTrail(Trail),
	New_Trail = [ [Action,X,Y,Orient] | Trail ],
	Knowledge = [gameStarted,
		     haveGold(NGolds),
	             myWorldSize(Max_X, Max_Y),
		     myPosition(New_X, New_Y, Orient),
		     myTrail(New_Trail)].

forwardStep(X, Y, east,  New_X, Y) :- New_X is (X+1).
forwardStep(X, Y, south, X, New_Y) :- New_Y is (Y-1).
forwardStep(X, Y, west,  New_X, Y) :- New_X is (X-1).
forwardStep(X, Y, north, X, New_Y) :- New_Y is (Y+1).
