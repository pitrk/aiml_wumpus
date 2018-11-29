/* -*- mode: Prolog; comment-column: 48 -*- */

act(Action, Knowledge) :- execute(Action, Knowledge).

execute(Action, Knowledge) :-
    get_all_not_discovered_neighbours([[1,2],[2,1]], [[1,1]], X), % X = [[1,2],[2,1]]
    Action = exit,
    Knowledge = [
        x(X)
    ].

%get_all_not_discovered_neighbours(AccessibleRooms, DiscoveredList, NotDiscoveredNeighbours).
get_all_not_discovered_neighbours([], _, []).
get_all_not_discovered_neighbours([First | Tail], DiscoveredList, NotDiscoveredNeighbours) :-
    (
        (member(First, DiscoveredList), TemporaryNotDiscoveredHere = []);
        (not(member(First, DiscoveredList)), TemporaryNotDiscoveredHere = First)
    ),
    get_all_not_discovered_neighbours(Tail, DiscoveredList, TemporaryNotDiscoveredRecursively),
    NotDiscoveredNeighbours2 = [TemporaryNotDiscoveredHere | TemporaryNotDiscoveredRecursively],
    delMember([], NotDiscoveredNeighbours2, NotDiscoveredNeighbours).

delMember(X, [], []) :- !.
delMember(X, [X|Xs], Y) :- !, delMember(X, Xs, Y).
delMember(X, [T|Xs], Y) :- !, delMember(X, Xs, Y2), append([T], Y2, Y).
