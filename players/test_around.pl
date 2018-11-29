/* -*- mode: Prolog; comment-column: 48 -*- */

act(Action, Knowledge) :- execute(Action, Knowledge).

execute(Action, Knowledge) :-
    accessible_rooms_around(5,5,5,5,ListRA), % [[2,3], [3,2], [2,1], [1,2]]
    Action = exit,
    Knowledge = [
        x(ListRA)
    ].

accessible_rooms_around(CurrentX, CurrentY, MaxX, MaxY, ListOfRoomsAround) :-
    northern_room(CurrentX, CurrentY, NorthernRoomX, NorthernRoomY),
    eastern_room(CurrentX, CurrentY, EasternRoomX, EasternRoomY),
    southern_room(CurrentX, CurrentY, SouthernRoomX, SouthernRoomY),
    western_room(CurrentX, CurrentY, WesternRoomX, WesternRoomY),
    % debug
    %ListOfRoomsAround = [[NorthernRoomX, NorthernRoomY],[EasternRoomX, EasternRoomY], [SouthernRoomX, SouthernRoomY], [WesternRoomX, WesternRoomY]].
    % gubed
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
