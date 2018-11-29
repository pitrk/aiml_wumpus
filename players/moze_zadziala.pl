/* -*- mode: Prolog; comment-column: 48 -*- */

act(Action, Knowledge) :-
	not(gameStarted),
	assert(gameStarted),

	worldSize(X, Y),
    assert(sizeOfWorld(X, Y)),
    assert(agentPosition(1, 1, east)),
    assert(goldAmount(0)),
    assert(executedMoves(0)),


    assert(navigationList([])),
    assert(discoveredList([[1,1]])),
    assert(dfsStack([])),
    assert(unsafePositions([])),
    assert(safePositions([[1,1]])),
    assert(shortestPathToExit([[1,1]])),

    act(Action, Knowledge).

act(Action, Knowledge) :- exit_if_danger_on_1_1(Action, Knowledge).
act(Action, Knowledge) :- exit_if_has_gold(Action, Knowledge).
act(Action, Knowledge) :- fake_gold_after_200_moves(Action, Knowledge).
act(Action, Knowledge) :- grab_gold(Action, Knowledge).
act(Action, Knowledge) :- navigation(Action, Knowledge).
act(Action, Knowledge) :- backtracking(Action, Knowledge).
act(Action, Knowledge) :- dfs_call(Action, Knowledge).
%debug
act(Action, Knowledge) :- generic_nothing(Action, Knowledge).

exit_if_danger_on_1_1(Action, Knowledge) :-
    sizeOfWorld(X, Y),
    agentPosition(AgentPositionX, AgentPositionY, AgentPositionTheta),
    goldAmount(GoldAmount),
    executedMoves(ExecutedMoves),
    navigationList(NavigationList),
    discoveredList(DiscoveredList),
    dfsStack(DfsStack),
    unsafePositions(UnsafePositions),
    safePositions(SafePositions),
    shortestPathToExit(ShortestPathToExit),

    AgentPositionX = 1, AgentPositionY = 1,
    (stench;breeze),

    accessible_rooms_around(X, Y, MaxX, MaxY, ListOfRoomsAround),
    NewUnsafePositions = [ListOfRoomsAround | UnsafePositions],

    Action = exit,
    NewExecutedMoves is ExecutedMoves + 1,

    Knowledge = [
        called_exited_on_danger, gameStarted,
        sizeOfWorld(X, Y),
        agentPosition(AgentPositionX, AgentPositionY, AgentPositionTheta),
        goldAmount(GoldAmount),
        executedMoves(NewExecutedMoves),
        navigationList(NavigationList),
        discoveredList(DiscoveredList),
        dfsStack(DfsStack),
        unsafePositions(NewUnsafePositions),
        safePositions(SafePositions),
        shortestPathToExit(ShortestPathToExit)
    ].

exit_if_has_gold(Action, Knowledge) :-
    sizeOfWorld(X, Y),
    agentPosition(AgentPositionX, AgentPositionY, AgentPositionTheta),
    goldAmount(GoldAmount),
    executedMoves(ExecutedMoves),
    navigationList(NavigationList),
    discoveredList(DiscoveredList),
    dfsStack(DfsStack),
    unsafePositions(UnsafePositions),
    safePositions(SafePositions),
    shortestPathToExit(ShortestPathToExit),

    AgentPositionX = 1, AgentPositionY = 1,
    GoldAmount > 0,

    Action = exit,
    NewExecutedMoves is ExecutedMoves + 1,

    Knowledge = [
        called_exited_with_gold, gameStarted,
        sizeOfWorld(X, Y),
        agentPosition(AgentPositionX, AgentPositionY, AgentPositionTheta),
        goldAmount(GoldAmount),
        executedMoves(NewExecutedMoves),
        navigationList(NavigationList),
        discoveredList(DiscoveredList),
        dfsStack(DfsStack),
        unsafePositions(UnsafePositions),
        safePositions(SafePositions),
        shortestPathToExit(ShortestPathToExit)
    ].

fake_gold_after_200_moves(Action, Knowledge) :-
    sizeOfWorld(MaxX, MaxY),
    agentPosition(X, Y, O),
    goldAmount(GoldAmount),
    executedMoves(ExecutedMoves),
    navigationList(NavigationList),
    discoveredList(DiscoveredList),
    dfsStack(DfsStack),
    unsafePositions(UnsafePositions),
    safePositions(SafePositions),
    shortestPathToExit(ShortestPathToExit),

    ExecutedMoves > 200,
    Action = grab,
    NewGoldAmount is GoldAmount + 1,
    NewExecutedMoves is ExecutedMoves + 1,
    % we need to shorten the shortest path list by this position
    NewShortestPathToExit = [_ | ShortestPathToExit],

    Knowledge = [
        called_fake_grab_gold,
        gameStarted,
        sizeOfWorld(MaxX, MaxY),
        agentPosition(X, Y, O),
        goldAmount(NewGoldAmount),
        executedMoves(NewExecutedMoves),
        navigationList(NavigationList),
        discoveredList(DiscoveredList),
        dfsStack(DfsStack),
        unsafePositions(UnsafePositions),
        safePositions(SafePositions),
        shortestPathToExit(NewShortestPathToExit)
    ].

grab_gold(Action, Knowledge) :-
    sizeOfWorld(MaxX, MaxY),
    agentPosition(X, Y, O),
    goldAmount(GoldAmount),
    executedMoves(ExecutedMoves),
    navigationList(NavigationList),
    discoveredList(DiscoveredList),
    dfsStack(DfsStack),
    unsafePositions(UnsafePositions),
    safePositions(SafePositions),
    shortestPathToExit(ShortestPathToExit),

    glitter,
    Action = grab,
    NewGoldAmount is GoldAmount + 1,
    NewExecutedMoves is ExecutedMoves + 1,
    % we need to shorten the shortest path list by this position
    NewShortestPathToExit = [_ | ShortestPathToExit],

    Knowledge = [
        called_grab_gold,
        gameStarted,
        sizeOfWorld(MaxX, MaxY),
        agentPosition(X, Y, O),
        goldAmount(NewGoldAmount),
        executedMoves(NewExecutedMoves),
        navigationList(NavigationList),
        discoveredList(DiscoveredList),
        dfsStack(DfsStack),
        unsafePositions(UnsafePositions),
        safePositions(SafePositions),
        shortestPathToExit(NewShortestPathToExit)
    ].

navigation(Action, Knowledge) :- % if there is something in navigationList
    sizeOfWorld(MaxX, MaxY),
    agentPosition(X, Y, O),
    goldAmount(GoldAmount),
    executedMoves(ExecutedMoves),
    navigationList(NavigationList),
    discoveredList(DiscoveredList),
    dfsStack(DfsStack),
    unsafePositions(UnsafePositions),
    safePositions(SafePositions),
    shortestPathToExit(ShortestPathToExit),

    length(NavigationList, Len), Len > 0,
    NavigationList = [FirstAction | NewNavigationList],
    Action = FirstAction,
    update_position_after_action(X, Y, O, FirstAction, NewX, NewY, NewO),
    NewExecutedMoves is ExecutedMoves + 1,

    Knowledge = [
        called_navigation,
        gameStarted,
        sizeOfWorld(MaxX, MaxY),
        agentPosition(NewX, NewY, NewO),
        goldAmount(GoldAmount),
        executedMoves(NewExecutedMoves),
        navigationList(NewNavigationList),
        discoveredList(DiscoveredList),
        dfsStack(DfsStack),
        unsafePositions(UnsafePositions),
        safePositions(SafePositions),
        shortestPathToExit(ShortestPathToExit)
    ].

backtracking(Action, Knowledge) :-
    sizeOfWorld(MaxX, MaxY),
    agentPosition(X, Y, O),
    goldAmount(GoldAmount),
    executedMoves(ExecutedMoves),
    navigationList(NavigationList),
    discoveredList(DiscoveredList),
    dfsStack(DfsStack),
    unsafePositions(UnsafePositions),
    safePositions(SafePositions),
    shortestPathToExit(ShortestPathToExit),

    GoldAmount > 0, % if we have gold
    ShortestPathToExit = [[PreviousPositionX, PreviousPositionY] | NewShortestPathToExit],
    procedure(X, Y, O, PreviousPositionX, PreviousPositionY, NavigationProcedures),
    TemporaryNavigationList = [NavigationProcedures | NavigationList], %add new nav procedures
    TemporaryNavigationList = [FirstAction | NewNavigationList], % remove first nav procedure
    Action = FirstAction, % first nav procedure execute
    update_position_after_action(X, Y, O, FirstAction, NewX, NewY, NewO),
    NewExecutedMoves is ExecutedMoves + 1,

    Knowledge = [
        called_backtracking,
        gameStarted,
        sizeOfWorld(MaxX, MaxY),
        agentPosition(NewX, NewY, NewO),
        goldAmount(GoldAmount),
        executedMoves(NewExecutedMoves),
        navigationList(NewNavigationList),
        discoveredList(DiscoveredList),
        dfsStack(DfsStack),
        unsafePositions(UnsafePositions),
        safePositions(SafePositions),
        shortestPathToExit(NewShortestPathToExit)
    ].

dfs_call(Action, Knowledge) :- % if unsafe on this position
    sizeOfWorld(MaxX, MaxY),
    agentPosition(X, Y, O),
    goldAmount(GoldAmount),
    executedMoves(ExecutedMoves),
    navigationList(NavigationList),
    discoveredList(DiscoveredList),
    dfsStack(DfsStack),
    unsafePositions(UnsafePositions),
    safePositions(SafePositions),
    shortestPathToExit(ShortestPathToExit),

    accessible_rooms_around(X, Y, MaxX, MaxY, ListOfRoomsAround),
    (breeze;stench), %if unsafe
    NewUnsafePositions = [ListOfRoomsAround | UnsafePositions], % mark all accessible neighbours unsafe (they are ok if they are on discoveredList)
    ShortestPathToExit = [_| NewShortestPathToExit], % remove this position from the shortest path list
    NewShortestPathToExit = [[PreviousPositionX, PreviousPositionY] | _], % get last position from the shortest path to backtrack to it
    procedure(X, Y, O, PreviousPositionX, PreviousPositionY, NavigationProcedures), % calculate moves to go there (it should be our neighbour)
    TemporaryNavigationList = [NavigationProcedures | NavigationList], %add new nav procedures
    TemporaryNavigationList = [FirstAction | NewNavigationList], % remove first nav procedure
    Action = FirstAction, % first nav procedure execute
    update_position_after_action(X, Y, O, FirstAction, NewX, NewY, NewO),
    NewExecutedMoves is ExecutedMoves + 1,

    Knowledge = [
        called_dfs_call_unsafe_here,
        gameStarted,
        sizeOfWorld(MaxX, MaxY),
        agentPosition(NewX, NewY, NewO),
        goldAmount(GoldAmount),
        executedMoves(NewExecutedMoves),
        navigationList(NewNavigationList),
        discoveredList(DiscoveredList),
        dfsStack(DfsStack),
        unsafePositions(NewUnsafePositions),
        safePositions(SafePositions),
        shortestPathToExit(NewShortestPathToExit)
    ].

dfs_call(Action, Knowledge) :- % if safe on this position and there are not discovered neighboursNotDiscoveredNeighbours
    sizeOfWorld(MaxX, MaxY),
    agentPosition(X, Y, O),
    goldAmount(GoldAmount),
    executedMoves(ExecutedMoves),
    navigationList(NavigationList),
    discoveredList(DiscoveredList),
    dfsStack(DfsStack),
    unsafePositions(UnsafePositions),
    safePositions(SafePositions),
    shortestPathToExit(ShortestPathToExit),

    %not(breeze), not(stench),
    accessible_rooms_around(X, Y, MaxX, MaxY, ListOfRoomsAround),
    append(ListOfRoomsAround, SafePositions, NewSafePositions),
    all_not_discovered_neighbours(ListOfRoomsAround, DiscoveredList, NotDiscoveredNeighbours),
    length(NotDiscoveredNeighbours, Len), Len > 0, % !!! If there are not discovered neighbours
    append(NotDiscoveredNeighbours, DfsStack, TemporaryDfsStack),
    TemporaryDfsStack = [[GotoX, GotoY] | NewDfsStack],
    NewDiscoveredList = [[GotoX, GotoY] | DiscoveredList],
    procedure(X, Y, O, GotoX, GotoY, NavigationProcedures),
    %append(NavigationProcedures, NavigationList, TemporaryNavigationList),
    %TemporaryNavigationList = [NavigationProcedures | NavigationList], %add new nav procedures
    %TemporaryNavigationList = [FirstAction | NewNavigationList], % remove first nav procedure

    %Action = FirstAction, % first nav procedure execute
    %update_position_after_action(X, Y, O, FirstAction, NewX, NewY, NewO),
    %NewExecutedMoves is ExecutedMoves + 1,
    %NewShortestPathToExit = [[[NewX, NewY]] | ShortestPathToExit ],
    Action = grab,
    Knowledge = [
        called_dfs_call_safe_and_something_to_explore,
        gameStarted,
        sizeOfWorld(MaxX, MaxY),
        %agentPosition(NewX, NewY, NewO),
        goldAmount(GoldAmount),
        %executedMoves(NewExecutedMoves),
        %navigationList(NewNavigationList),
        %discoveredList(NewDiscoveredList),
        %dfsStack(NewDfsStack),
        %unsafePositions(UnsafePositions),
        %safePositions(NewSafePositions),
        %shortestPathToExit(NewShortestPathToExit)
        listofroomsaround(ListOfRoomsAround),
        notdiscoveredneighbours(NotDiscoveredNeighbours),
        %notdiscoveredneighbours2(NotDiscoveredNeighbours2),
        newsafepositions(NewSafePositions),
        leng(Len),
        temdefstack(TemporaryDfsStack),
        first(GotoX, GotoY),
        navproc(NavigationProcedures),
        xyo(X, Y, O, GotoX, GotoY)
    ].

dfs_call(Action, Knowledge) :- % if safe on this position and there are no not discovered neighbours
    sizeOfWorld(MaxX, MaxY),
    agentPosition(X, Y, O),
    goldAmount(GoldAmount),
    executedMoves(ExecutedMoves),
    navigationList(NavigationList),
    discoveredList(DiscoveredList),
    dfsStack(DfsStack),
    unsafePositions(UnsafePositions),
    safePositions(SafePositions),
    shortestPathToExit(ShortestPathToExit),

    %not(breeze), not(stench),
    accessible_rooms_around(X, Y, MaxX, MaxY, ListOfRoomsAround),
    NewSafePositions = [ListOfRoomsAround | SafePositions],
    all_not_discovered_neighbours(ListOfRoomsAround, DiscoveredList, NotDiscoveredNeighbours),
    length(NotDiscoveredNeighbours, Len), Len == 0, % no not yet discovered neighbours - go back, nothing to do here
    NewDiscoveredList = [[[X, Y]] | DiscoveredList], % at least save that we've been here
    ShortestPathToExit = [_| NewShortestPathToExit], % remove this position from the shortest path list
    NewShortestPathToExit = [[PreviousPositionX, PreviousPositionY] | _], % get last position from the shortest path to backtrack to it
    procedure(X, Y, O, PreviousPositionX, PreviousPositionY, NavigationProcedures), % calculate moves to go there (it should be our neighbour)
    TemporaryNavigationList = [NavigationProcedures | NavigationList], %add new nav procedures
    TemporaryNavigationList = [FirstAction | NewNavigationList], % remove first nav procedure
    Action = FirstAction, % first nav procedure execute
    update_position_after_action(X, Y, O, FirstAction, NewX, NewY, NewO),
    NewExecutedMoves is ExecutedMoves + 1,

    Knowledge = [
        called_dfs_call_safe_and_nothing_to_explore,
        gameStarted,
        sizeOfWorld(MaxX, MaxY),
        agentPosition(NewX, NewY, NewO),
        goldAmount(GoldAmount),
        executedMoves(NewExecutedMoves),
        navigationList(NewNavigationList),
        discoveredList(NewDiscoveredList),
        dfsStack(DfsStack),
        unsafePositions(UnsafePositions),
        safePositions(NewSafePositions),
        shortestPathToExit(NewShortestPathToExit)
    ].

generic_nothing(Action, Knowledge) :-
    sizeOfWorld(MaxX, MaxY),
    agentPosition(X, Y, O),
    goldAmount(GoldAmount),
    executedMoves(ExecutedMoves),
    navigationList(NavigationList),
    discoveredList(DiscoveredList),
    dfsStack(DfsStack),
    unsafePositions(UnsafePositions),
    safePositions(SafePositions),
    shortestPathToExit(ShortestPathToExit),

    Action = exit,
    NewExecutedMoves is ExecutedMoves + 1,

    Knowledge = [
        called_generic_nothing, gameStarted,
        sizeOfWorld(MaxX, MaxY),
        agentPosition(X, Y, O),
        goldAmount(GoldAmount),
        executedMoves(NewExecutedMoves),
        navigationList(NavigationList),
        discoveredList(DiscoveredList),
        dfsStack(DfsStack),
        unsafePositions(UnsafePositions),
        safePositions(SafePositions),
        shortestPathToExit(ShortestPathToExit)
    ].



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
% delete_one(ElementToRemove, ListToRemoveElementFrom, ListWithoutRemovedElement)
delete_one(_, [], []).
delete_one(Term, [Term|Tail], Tail).
delete_one(Term, [Head|Tail], [Head|Result]) :-
  delete_one(Term, Tail, Result).

remove_n_elements_in_front(ListToShorten, 0, ListToShorten).
remove_n_elements_in_front(ListToShorten, N, ShortedList) :-
    ListToShorten = [_|Shorter],
    NewN is N - 1,
    remove_n_elements_in_front(Shorter, NewN, ShortedList).

procedure(X, Y, north, X,    NewY, ActionList) :- NewY is (Y+1), ActionList = [moveForward].
procedure(X, Y, east,  NewX, Y,    ActionList) :- NewX is (X+1), ActionList = [moveForward].
procedure(X, Y, south, X,    NewY, ActionList) :- NewY is (Y-1), ActionList = [moveForward].
procedure(X, Y, west,  NewX, Y,    ActionList) +- NewX is (X-1), ActionList = [moveForward].

procedure(X, Y, north, NewX, Y,    ActionList) :- NewX is (X-1), ActionList = [turnLeft, moveForward].
procedure(X, Y, east,  X,    NewY, ActionList) :- NewY is (Y+1), ActionList = [turnLeft, moveForward].
procedure(X, Y, south, NewX, Y,    ActionList) :- NewX is (X+1), ActionList = [turnLeft, moveForward].
procedure(X, Y, west,  X,    NewY, ActionList) :- NewY is (Y-1), ActionList = [turnLeft, moveForward].

procedure(X, Y, north, NewX, Y,    ActionList) :- NewX is (X+1), ActionList = [turnRight, moveForward].
procedure(X, Y, east,  X,    NewY, ActionList) :- NewY is (Y-1), ActionList = [turnRight, moveForward].
procedure(X, Y, south, NewX, Y,    ActionList) :- NewX is (X-1), ActionList = [turnRight, moveForward].
procedure(X, Y, west,  X,    NewY, ActionList) :- NewY is (Y+1), ActionList = [turnRight, moveForward].

procedure(X, Y, north, X,    NewY, ActionList) :- NewY is (Y-1), ActionList = [turnRight, turnRight, moveForward].
procedure(X, Y, east,  NewX, Y,    ActionList) :- NewX is (X-1), ActionList = [turnRight, turnRight, moveForward].
procedure(X, Y, south, X,    NewY, ActionList) :- NewY is (Y+1), ActionList = [turnRight, turnRight, moveForward].
procedure(X, Y, west,  NewX, Y,    ActionList) :- NewX is (X+1), ActionList = [turnRight, turnRight, moveForward].

% update_position_after_action(CurrentX, CurrentY, CurrentO, Action, NewX, NewY, NewO)
% TODO: Poprawić jak te powyżej
update_position_after_action(X, Y, north, moveForward, X, Y+1, north).
update_position_after_action(X, Y, east, moveForward, X+1, Y, east).
update_position_after_action(X, Y, south, moveForward, X, Y-1, south).
update_position_after_action(X, Y, west, moveForward, X-1, Y, west).

update_position_after_action(X, Y, north, turnRight, X, Y, east).
update_position_after_action(X, Y, east, turnRight, X, Y, south).
update_position_after_action(X, Y, south, turnRight, X, Y, west).
update_position_after_action(X, Y, west, turnRight, X, Y, north).

update_position_after_action(X, Y, north, turnLeft, X, Y, west).
update_position_after_action(X, Y, east, turnLeft, X, Y, north).
update_position_after_action(X, Y, south, turnLeft, X, Y, east).
update_position_after_action(X, Y, west, turnLeft, X, Y, south).

%all_not_discovered_neighbours(AccessibleRooms, DiscoveredList, NotDiscoveredNeighbours).
all_not_discovered_neighbours([], _, []).
all_not_discovered_neighbours([First | Tail], DiscoveredList, NotDiscoveredNeighbours) :-
    (
        (member(First, DiscoveredList), TemporaryNotDiscoveredHere = []);
        (not(member(First, DiscoveredList)), TemporaryNotDiscoveredHere = First)
    ),
    all_not_discovered_neighbours(Tail, DiscoveredList, TemporaryNotDiscoveredRecursively),
    NotDiscoveredNeighbours2 = [TemporaryNotDiscoveredHere | TemporaryNotDiscoveredRecursively],
    delMember([], NotDiscoveredNeighbours2, NotDiscoveredNeighbours).

delMember(X, [], []) :- !.
delMember(X, [X|Xs], Y) :- !, delMember(X, Xs, Y).
delMember(X, [T|Xs], Y) :- !, delMember(X, Xs, Y2), append([T], Y2, Y).
