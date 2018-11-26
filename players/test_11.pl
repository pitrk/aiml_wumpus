/* -*- mode: Prolog; comment-column: 48 -*- */

% Random moves
act(Action, Knowledge) :- turn_left(Action, Knowledge).
act(Action, Knowledge) :- turn_right(Action, Knowledge).
act(Action, Knowledge) :- go_forward(Action, Knowledge).
act(Action, Knowledge) :- else_turn_left(Action, Knowledge).

turn_left(Action, Knowledge) :-
    rand_float(X), X<0.4,
    (lastAction(moveForward); bump),
    Action = turnLeft,
    Knowledge = [lastAction(turnLeft)].

turn_right(Action, Knowledge) :-
    rand_float(X), X<0.4,
    (lastAction(moveForward); bump),
    Action = turnRight,
    Knowledge = [lastAction(turnLeft)].

go_forward(Action, Knowledge) :-
    not(bump),
    Action = moveForward,
    Knowledge = [lastAction(moveForward)].

else_turn_left(Action, Knowledge) :-
    Action = turnLeft,
    Knowledge = [lastAction(turnLeft)].
