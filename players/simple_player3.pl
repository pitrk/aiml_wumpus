/* -*- mode: Prolog; comment-column: 48 -*- */

/****************************************************************************
 *
 * Copyright (c) 2012 Witold Paluszynski
 *
 * I grant everyone the right to copy this program in whole or part, and
 * to use it for any purpose, provided the source is properly acknowledged.
 *
 * Udzielam kazdemu prawa do kopiowania tego programu w calosci lub czesci,
 * i wykorzystania go w dowolnym celu, pod warunkiem zacytowania zrodla.
 *
 ****************************************************************************/


/*
  This program implements a simple agent strategy for the wumpus world.
  The agent ignores all dangers of the wumpus world.
  The strategy is to go forward, turn left if bumped into a wall,
  but first of all pick up gold if stumbled upon it.

  Also demonstrates how to keep track of her own position and orientation.
  The agent assumes that the starting point is (1,1) and orientation "east".
*/

% auxiliary initial action generating rule
act(Action, Knowledge) :-

	% To avoid looping on act/2.
	not(gameStarted),
	assert(gameStarted),

	% Creating initial knowledge
	worldSize(X,Y),				%this is given
	assert(myWorldSize(X,Y)),
	assert(myPosition(1, 1, east)),		%this we assume by default
	act(Action, Knowledge).

% standard action generating rules
% this is our agent's algorithm, the rules will be tried in order
act(Action, Knowledge) :- pick_up_gold(Action, Knowledge).
act(Action, Knowledge) :- turn_if_bump(Action, Knowledge).
act(Action, Knowledge) :- else_move_on(Action, Knowledge).

pick_up_gold(Action, Knowledge) :-
	glitter,
	Action = grab,			    %this is easy, we are sitting on it
	myWorldSize(Max_X, Max_Y),
	myPosition(X, Y, Orient),
	Knowledge = [gameStarted,
		     myWorldSize(Max_X, Max_Y),	%the position stays the same
		     myPosition(X, Y, Orient)].

turn_if_bump(Action, Knowledge) :-
	bump,
	Action = turnLeft,			%always successful
	myWorldSize(Max_X,Max_Y),
	myPosition(X, Y, Orient),		%this is a wrong position
	retractStep(X, Y, Orient, Real_X, Real_Y),	%since we bumped a wall
	shiftOrient(Orient, NewOrient),		%always successful
	Knowledge = [gameStarted,
	             myWorldSize(Max_X, Max_Y),
		     myPosition(Real_X, Real_Y, NewOrient)].

retractStep(X, Y, east, Real_X, Y) :- Real_X is (X-1).
retractStep(X, Y, south, X, Real_Y) :- Real_Y is (Y+1).
retractStep(X, Y, west, Real_X, Y) :- Real_X is (X+1).
retractStep(X, Y, north, X, Real_Y) :- Real_Y is (Y-1).

shiftOrient(east, north).
shiftOrient(north, west).
shiftOrient(west, south).
shiftOrient(south, east).

else_move_on(Action, Knowledge) :-
	Action = moveForward,			%this will fail on a wall
	myWorldSize(Max_X,Max_Y),
	myPosition(X, Y, Orient),
	forwardStep(X, Y, Orient, New_X, New_Y),
	Knowledge = [gameStarted,
	             myWorldSize(Max_X, Max_Y),
		     myPosition(New_X, New_Y, Orient)].

forwardStep(X, Y, east, New_X, Y) :- New_X is (X+1).
forwardStep(X, Y, south, X, New_Y) :- New_Y is (Y-1).
forwardStep(X, Y, west, New_X, Y) :- New_X is (X-1).
forwardStep(X, Y, north, X, New_Y) :- New_Y is (Y+1).

% note this agent will know when she is against the wall,
% but she ignores this knowledge and just moves forward

