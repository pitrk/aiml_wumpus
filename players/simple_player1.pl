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
  The strategy is to go forward, and turn left if bumped into a wall.
*/

% standard action generating rules
% this is our agent's algorithm, the rules will be tried in order
act(Action, Knowledge) :- turn_if_bump(Action, Knowledge).
act(Action, Knowledge) :- else_move_on(Action, Knowledge).

turn_if_bump(Action, Knowledge) :-
	bump,
	Action = turnLeft,			%always successful
	Knowledge = [].

else_move_on(Action, Knowledge) :-
	Action = moveForward,			%this will fail on a wall
	Knowledge = [].				%but she doesn't care

