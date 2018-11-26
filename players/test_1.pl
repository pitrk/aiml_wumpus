/* -*- mode: Prolog; comment-column: 48 -*- */

act(Action, Knowledge) :-
	not(gameStarted),
	assert(gameStarted),

	worldSize(X,Y),
	assert(myWorldSize(X,Y)),
	assert(myPosition(1, 1, east)),
	assert(hasGold(0)),
    assert(executedMoves(0)),
	act(Action, Knowledge).


act(Action, Knowledge) :- exit_if_danger_on_1_1(Action, Knowledge).
act(Action, Knowledge) :- grab_if_gold(Action, Knowledge).
act(Action, Knowledge) :- general_exit(Action, Knowledge).
act(Action, Knowledge) :- random_turn_left(Action, Knowledge).
act(Action, Knowledge) :- random_turn_right(Action, Knowledge).
act(Action, Knowledge) :- random_go_forward(Action, Knowledge).
act(Action, Knowledge) :- else_turn_left(Action, Knowledge).

exit_if_danger_on_1_1(Action, Knowledge) :-
    myWorldSize(X, Y),
    myPosition(PositionX, PositionY, PositionTheta),
    hasGold(Gold),
    executedMoves(ExecutedMoves),
    lastAction(LastAction),

    (stench;breeze),
    PositionX = 1, PositionY = 1,

    Action = exit,
    ExecutedMoves2 is ExecutedMoves + 1,
    Knowledge = [
        gameStarted,
        exited_on_danger,
        executedMoves(ExecutedMoves2),
        lastAction(LastAction),
        myWorldSize(X, Y),
        myPosition(PositionX, PositionY, PositionTheta),
        hasGold(Gold)
    ].

grab_if_gold(Action, Knowledge) :-
    myWorldSize(X, Y),
    myPosition(PositionX, PositionY, PositionTheta),
    hasGold(Gold),
    executedMoves(ExecutedMoves),
    lastAction(LastAction),

    glitter,

    Action = grab,
    ExecutedMoves2 is ExecutedMoves + 1,
    Gold2 is 1,

    Knowledge = [
        gameStarted,
        myWorldSize(X, Y),
        myPosition(PositionX, PositionY, PositionTheta),
        executedMoves(ExecutedMoves2),
        lastAction(LastAction),
        myWorldSize(X, Y),
        myPosition(PositionX, PositionY, PositionTheta),
        hasGold(Gold2)
    ].


general_exit(Action, Knowledge) :-
    myWorldSize(X, Y),
    myPosition(PositionX, PositionY, PositionTheta),
    hasGold(Gold),
    executedMoves(ExecutedMoves),
    lastAction(LastAction),

    PositionX = 1, PositionY = 1,

	Action = exit,
	Gold > 0,
    ExecutedMoves2 is ExecutedMoves + 1,
	Knowledge = [
        gameStarted,
        exited_general,
        executedMoves(ExecutedMoves2),
        lastAction(LastAction),
        myWorldSize(X, Y),
        myPosition(PositionX, PositionY, PositionTheta),
        hasGold(Gold)
    ].

random_turn_left(Action, Knowledge) :-
    executedMoves(ExecutedMoves),
    myWorldSize(X, Y),
    myPosition(PositionX, PositionY, PositionTheta),
    hasGold(Gold),

    (not(stench);not(breeze)),
    rand_float(X), X<0.4,
    (lastAction(moveForward); bump),
    Action = turnLeft,

    left(PositionTheta, NewPositionTheta),
    ExecutedMoves2 is ExecutedMoves + 1,
    Knowledge = [
        gameStarted,
        executedMoves(ExecutedMoves2),
        lastAction(turnLeft),
        myWorldSize(X, Y),
        myPosition(PositionX, PositionY, NewPositionTheta),
        hasGold(Gold)
    ].

random_turn_right(Action, Knowledge) :-
    executedMoves(ExecutedMoves),
    myWorldSize(X, Y),
    myPosition(PositionX, PositionY, PositionTheta),
    hasGold(Gold),

    (not(stench);not(breeze)),
    rand_float(X), X<0.4,
    (lastAction(moveForward); bump),
    Action = turnRight,

    right(PositionTheta, NewPositionTheta),
    ExecutedMoves2 is ExecutedMoves + 1,
    Knowledge = [
        gameStarted,
        executedMoves(ExecutedMoves2),
        lastAction(turnLeft),
        myWorldSize(X, Y),
        myPosition(PositionX, PositionY, NewPositionTheta),
        hasGold(Gold)
    ].

random_go_forward(Action, Knowledge) :-
    executedMoves(ExecutedMoves),
    myWorldSize(X, Y),
    myPosition(PositionX, PositionY, PositionTheta),
    hasGold(Gold),

    not(bump),
    (not(stench);not(breeze)),
    Action = moveForward,

    ExecutedMoves2 is ExecutedMoves + 1,
    positionInFrontOf(PositionX, PositionY, PositionTheta, NewPositionX, NewPositionY),
    Knowledge = [
        gameStarted,
        executedMoves(ExecutedMoves2),
        lastAction(moveForward),
        myWorldSize(X, Y),
        myPosition(NewPositionX, NewPositionY, PositionTheta),
        hasGold(Gold)
    ].

else_turn_left(Action, Knowledge) :-
    executedMoves(ExecutedMoves),
    myWorldSize(X, Y),
    myPosition(PositionX, PositionY, PositionTheta),
    hasGold(Gold),

    Action = turnLeft,

    left(PositionTheta, NewPositionTheta),
    ExecutedMoves2 is ExecutedMoves + 1,
    Knowledge = [
        gameStarted,
        executedMoves(ExecutedMoves2),
        lastAction(turnLeft),
        myWorldSize(X, Y),
        myPosition(PositionX, PositionY, NewPositionTheta),
        hasGold(Gold)
    ].

positionInFrontOf(X, Y, east,  New_X, Y) :- New_X is (X+1).
positionInFrontOf(X, Y, south, X, New_Y) :- New_Y is (Y-1).
positionInFrontOf(X, Y, west,  New_X, Y) :- New_X is (X-1).
positionInFrontOf(X, Y, north, X, New_Y) :- New_Y is (Y+1).

left(north, west).
left(west, south).
left(south, east).
left(east, north).

right(north, east).
right(east, south).
right(south, west).
right(west, north).
