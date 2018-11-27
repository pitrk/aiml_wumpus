/* -*- mode: Prolog; comment-column: 48 -*- */

act(Action, Knowledge) :-
	not(gameStarted),
	assert(gameStarted),

	worldSize(X, Y),
    assert(sizeOfWorld(X, Y)),
    assert(agentPosition(1, 1, facingEast)),
    assert(goldAmount(0)),
    assert(visitedPositions([[1,1]])),
    assert(executedMoves(0)),

    assert(navigationList([])),
    assert(discoveredList([])),
    assert(dfsStack([])),
    assert(unsafePositions([])),

    act(Action, Knowledge).

act(Action, Knowledge) :- exit_if_danger_on_1_1(Action, Knowledge).
act(Action, Knowledge) :- exit_if_has_gold(Action, Knowledge).
% act(Action, Knowledge) :- forward(Action, Knowledge).

exit_if_danger_on_1_1(Action, Knowledge) :-
    sizeOfWorld(X, Y),
    agentPosition(AgentPositionX, AgentPositionY, AgentPositionTheta),
    goldAmount(GoldAmount),
    arrowsAmount(ArrowsAmount),
    visitedPositions(VisitedPositions),
    executedMoves(ExecutedMoves),
    lastActions(LastActionsList),

    AgentPositionX = 1, AgentPositionY = 1,
    (stench;breeze),

    Action = exit,
    ExecutedMovesNew is ExecutedMoves + 1,

    Knowledge = [
        exited_on_danger, gameStarted,
        sizeOfWorld(X, Y),
        agentPosition(AgentPositionX, AgentPositionY, AgentPositionTheta),
        goldAmount(GoldAmount),
        arrowsAmount(ArrowsAmount),
        visitedPositions(VisitedPositions),
        executedMoves(ExecutedMovesNew),
        lastActions(LastActionsList)
    ].

exit_if_has_gold(Action, Knowledge) :-
    sizeOfWorld(X, Y),
    agentPosition(AgentPositionX, AgentPositionY, AgentPositionTheta),
    goldAmount(GoldAmount),
    arrowsAmount(ArrowsAmount),
    visitedPositions(VisitedPositions),
    executedMoves(ExecutedMoves),
    lastActions(LastActionsList),

    AgentPositionX = 1, AgentPositionY = 1,
    GoldAmount > 0,

    Action = exit,
    ExecutedMovesNew is ExecutedMoves + 1,

    Knowledge = [
        exited_with_gold, gameStarted,
        sizeOfWorld(X, Y),
        agentPosition(AgentPositionX, AgentPositionY, AgentPositionTheta),
        goldAmount(GoldAmount),
        arrowsAmount(ArrowsAmount),
        visitedPositions(VisitedPositions),
        executedMoves(ExecutedMovesNew),
        lastActions(LastActionsList)
    ].

dfs_call(Action, Knowledge) :- % if unsafe on this position
    sizeOfWorld(MaxX, MaxY),
    agentPosition(X, Y, O),
    goldAmount(GoldAmount),
    visitedPositions(VisitedPositions),
    executedMoves(ExecutedMoves),
    navigationList(NavigationList),
    discoveredList(DiscoveredList),
    dfsStack(DfsStack),
    unsafePositions(UnsafePositions),

    accessible_rooms_around(X, Y, MaxX, MaxY, ListOfRoomsAround),
    (breeze;stench),
    NewUnsafePositions = [ListOfRoomsAround | UnsafePositions],
    NewNavigationList = [[turnRight, moveForward] | NavigationList],

    Act = turnRight,
    right(O, NewO),
    NewExecutedMoves is ExecutedMoves + 1,

    Knowledge = [
        sizeOfWorld(MaxX, MaxY),
        agentPosition(X, Y, NewO),
        goldAmount(GoldAmount),
        visitedPositions(VisitedPositions),
        executedMoves(NewExecutedMoves),
        navigationList(NewNavigationList),
        discoveredList(DiscoveredList),
        dfsStack(DfsStack),
        unsafePositions(NewUnsafePositions)
    ]



% forward(Action, Knowledge) :-
%     sizeOfWorld(X, Y),
%     agentPosition(AgentPositionX, AgentPositionY, AgentPositionTheta),
%     goldAmount(GoldAmount),
%     arrowsAmount(ArrowsAmount),
%     visitedPositions(VisitedPositions),
%     executedMoves(ExecutedMoves),
%     lastActions(LastActionsList),
%
%
%     Action = moveForward,
%     ExecutedMovesNew is ExecutedMoves + 1,
%     add_new_action_and_remove_last(LastActionsList, moveForward, LastActionsListNew),
%     accessible_rooms_around(AgentPositionX, AgentPositionY, X, Y, ListOfRoomsAround),
%     %delete_one([1,2], ListOfRoomsAround, NewListOfRoomsAround),
%
%     Knowledge = [
%         gameStarted,
%         sizeOfWorld(X, Y),
%         agentPosition(AgentPositionX, AgentPositionY, AgentPositionTheta),
%         goldAmount(GoldAmount),
%         arrowsAmount(ArrowsAmount),
%         visitedPositions(VisitedPositions),
%         executedMoves(ExecutedMovesNew),
%         lastActions(LastActionsListNew),
%         listOfRoomsAround(ListOfRoomsAround)
%         %newListOfRoomsAround(NewListOfRoomsAround),
%     ].

positionInFrontOf(X, Y, east,  New_X, Y) :- New_X is (X+1).
positionInFrontOf(X, Y, south, X, New_Y) :- New_Y is (Y-1).
positionInFrontOf(X, Y, west,  New_X, Y) :- New_X is (X-1).
positionInFrontOf(X, Y, north, X, New_Y) :- New_Y is (Y+1).

positionOnLeftOf(X, Y, north, New_X, Y) :- New_X is (X-1).
positionOnLeftOf(X, Y, east, X, New_Y) :- New_Y is (Y+1).
positionOnLeftOf(X, Y, south, New_X, Y) :- New_X is (X+1).
positionOnLeftOf(X, Y, west, X, New_Y) :- New_Y is (Y-1).

positionOnRightOf(X, Y, north, New_X, Y) :- New_X is (X+1).
positionOnRightOf(X, Y, east, X, New_Y) :- New_Y is (Y-1).
positionOnRightOf(X, Y, south, New_X, Y) :- New_X is (X-1).
positionOnRightOf(X, Y, west, X, New_Y) :- New_Y is (Y+1).

positionBehindOf(X, Y, north, X, New_Y) :- New_Y is (Y-1).
positionBehindOf(X, Y, east, New_X, Y) :- New_X is (X-1).
positionBehindOf(X, Y, south, X, New_Y) :- New_Y is (Y+1).
positionBehindOf(X, Y, west, New_X, Y) :- New_X is (X+1).

left(north, west).
left(west, south).
left(south, east).
left(east, north).

right(north, east).
right(east, south).
right(south, west).
right(west, north).

add_new_action_and_remove_last(ActionList, NewAction, NewActionList) :-
     length(ActionList, Len), Len < 4,
     NewActionList = [[NewAction] | ActionList].
add_new_action_and_remove_last(ActionList, NewAction, NewActionList) :-
    append(WithoutLast, [_], ActionList),
    NewActionList = [[NewAction] | WithoutLast].

accessible_rooms_around(CurrentX, CurrentY, MaxX, MaxY, ListOfRoomsAround) :-
    northern_room(CurrentX, CurrentY, NorthernRoomX, NorthernRoomY),
    eastern_room(CurrentX, CurrentY, EasternRoomX, EasternRoomY),
    southern_room(CurrentX, CurrentY, SouthernRoomX, SouthernRoomY),
    western_room(CurrentX, CurrentY, WesternRoomX, WesternRoomY),
    (
        (coordinates_exist(NorthernRoomX, NorthernRoomY, MaxX, MaxY), T1=[[NorthernRoomX, NorthernRoomY]]);
        (not(coordinates_exist(NorthernRoomX, NorthernRoomY, MaxX, MaxY)), T1=[])
    ),
    (
        (coordinates_exist(EasternRoomX, EasternRoomY, MaxX, MaxY), T2=[[EasternRoomX, EasternRoomY] | T1]);
        (not(coordinates_exist(EasternRoomX, EasternRoomY, MaxX, MaxY)), T2=T1)
    ),
    (
        (coordinates_exist(SouthernRoomX, SouthernRoomY, MaxX, MaxY), T3=[[SouthernRoomX, SouthernRoomY] | T2]);
        (not(coordinates_exist(SouthernRoomX, SouthernRoomY, MaxX, MaxY)), T3=T2)
    ),
    (
        (coordinates_exist(WesternRoomX, WesternRoomY, MaxX, MaxY), ListOfRoomsAround=[[WesternRoomX, WesternRoomY] | T3]);
        (not(coordinates_exist(WesternRoomX, WesternRoomY, MaxX, MaxY)), ListOfRoomsAround=T3)
    ).

coordinates_exist(X, Y, MaxX, MaxY) :-
    X>0, X=<MaxX, Y>0, Y=<MaxY.

northern_room(X, Y, NorthX, NorthY) :-
    NorthX is X, NorthY is Y + 1.

eastern_room(X, Y, EastX, EastY) :-
    EastX is X + 1, EastY is Y.

southern_room(X, Y, SouthX, SouthY) :-
    SouthX is X, SouthY is Y - 1.

western_room(X, Y, WestX, WestY) :-
    WestX is X - 1, WestY is Y.

% Source: https://stackoverflow.com/a/15857442/8168925
delete_one(_, [], []).
delete_one(Term, [Term|Tail], Tail).
delete_one(Term, [Head|Tail], [Head|Result]) :-
  delete_one(Term, Tail, Result).

remove_n_elements_in_front(ListToShorten, 0, ListToShorten).
remove_n_elements_in_front(ListToShorten, N, ShortedList) :-
    ListToShorten = [_|Shorter],
    NewN is N - 1,
    remove_n_elements_in_front(Shorter, NewN, ShortedList).

procedure(X, Y, north, X,   Y+1, [moveForward]).
procedure(X, Y, east,  X+1, Y,   [moveForward]).
procedure(X, Y, south, X,   Y-1, [moveForward]).
procedure(X, Y, west,  X-1, Y,   [moveForward]).

procedure(X, Y, north, X-1, Y,   [turnLeft, moveForward]).
procedure(X, Y, east,  X,   Y+1, [turnLeft, moveForward]).
procedure(X, Y, south, X+1, Y,   [turnLeft, moveForward]).
procedure(X, Y, west,  X,   Y-1, [turnLeft, moveForward]).

procedure(X, Y, north, X+1, Y,   [turnRight, moveForward]).
procedure(X, Y, east,  X,   Y-1, [turnRight, moveForward]).
procedure(X, Y, south, X-1, Y,   [turnRight, moveForward]).
procedure(X, Y, west,  X,   Y+1, [turnRight, moveForward]).

procedure(X, Y, north, X,   Y-1, [turnRight, turnRight, moveForward]).
procedure(X, Y, east,  X-1, Y,   [turnRight, turnRight, moveForward]).
procedure(X, Y, south, X,   Y+1, [turnRight, turnRight, moveForward]).
procedure(X, Y, west,  X+1, Y,   [turnRight, turnRight, moveForward]).
