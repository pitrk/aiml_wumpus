/* -*- mode: Prolog; comment-column: 48 -*- */

/****************************************************************************
 *
 * Copyright (c) 2013 Witold Paluszynski
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
  The strategy is to go forward along the perimeter,
  turn left when reached the opposing wall,
  but first of all pick up gold if stumbled upon it,
  and exit the game if at home with gold.
  This version registers all steps on a stack, and uses it to reverse
  the actions after having found gold, thus properly returning home.

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

	assert(wumpus_dead(0)),
	assert(shooted_arrows(0)),
	assert(position_shooted(0,0)),
	assert(moves(0)),
	assert(save_list([])),
	assert(temporary_unsafe([])),
	assert(turning_back(0)),
	assert(back_actions([])),

	assert(myWorldSize(X,Y)),
	assert(myPosition(1, 1, east)),		%this we assume by default
	assert(myTrail([])),
	assert(haveGold(0)),
	act(Action, Knowledge).

% standard action generating rules
% this is our agent's algorithm, the rules will be tried in order
act(Action, Knowledge) :- exit_if_breeze_on_start(Action, Knowledge).
% hole prevention ========

% =======================
% / / / / / / / / / / / /
% wumpus prevention ======
% act(Action, Knowledge) :- go_if_stench(Action, Knowledge). %if arrow shooted - go
act(Action, Knowledge) :- save_if_wumpus_dead(Action, Knowledge). %if wumpus dead - save it

% =======================
act(Action, Knowledge) :- exit_if_home(Action, Knowledge). %if at home with gold
act(Action, Knowledge) :- go_back_step(Action, Knowledge). %if have gold elsewhere
act(Action, Knowledge) :- pick_up_gold(Action, Knowledge). %if just found gold
act(Action, Knowledge) :- exit_if_world_is_too_hard(Action, Knowledge). %fake grab nugget
%hole prevention
act(Action, Knowledge) :- backing(Action, Knowledge).
act(Action, Knowledge) :- if_breeze_turn_back(Action, Knowledge).

act(Action, Knowledge) :- turn_left_if_corner(Action, Knowledge). %if against the wall
act(Action, Knowledge) :- turn_right_if_corner(Action, Knowledge). %if against the wall
act(Action, Knowledge) :- turn_if_wall(Action, Knowledge).
%wumpus prevention
act(Action, Knowledge) :- shoot_if_stench(Action, Knowledge). %if stench - shoot

act(Action, Knowledge) :- else_move_on(Action, Knowledge). %otherwise



% hole methods==============================================
% backward_if_breeze(Action, Knowledge) :-
% 	breeze,
% 	haveGold(NGolds),
% 	myWorldSize(Max_X, Max_Y),
% 	myPosition(X, Y, Orient),
% 	myTrail(Trail),
% 	wumpus_dead(IsDead),
% 	shooted_arrows(Arrows),
% 	position_shooted(Shooted_X, Shooted_Y),
%
%
%   Action = turnLeft,
% 	moves(Move),
% 	AmountMoves is Move + 1,
% 	Knowledge = [gameStarted, wumpus_dead(IsDead), shooted_arrows(Arrows), position_shooted(Shooted_X, Shooted_Y), moves(AmountMoves),
% 							 haveGold(NGolds), myWorldSize(Max_X, Max_Y), myPosition(X, Y, Orient), myTrail(Trail)].
%
%
%
%
%
% keep_save_fields(Action, Knowledge) :-
% 	haveGold(NGolds),
% 	myWorldSize(Max_X, Max_Y),
% 	myPosition(X, Y, Orient),
% 	myTrail(Trail),
% 	wumpus_dead(IsDead),
% 	shooted_arrows(Arrows),
% 	position_shooted(Shooted_X, Shooted_Y),
%   myTrail([ [moveForward,Last_X,Last_Y,Orient] | Trail_Tail ]),
% 	not(breeze),
%
%
%   Action = turnLeft,
% 	moves(Move),
% 	AmountMoves is Move + 1,
% 	Knowledge = [gameStarted, wumpus_dead(IsDead), shooted_arrows(Arrows), position_shooted(Shooted_X, Shooted_Y), moves(AmountMoves),
% 							 haveGold(NGolds), myWorldSize(Max_X, Max_Y), myPosition(X, Y, Orient), myTrail(Trail)].


% wumpus methods=============================================
shoot_if_stench(Action, Knowledge) :-
	stench,
	haveGold(NGolds),
	myWorldSize(Max_X, Max_Y),
	myPosition(X, Y, Orient),
	myTrail(Trail),
	wumpus_dead(IsDead), IsDead < 1,
	shooted_arrows(Arrows), Arrows < 1,
  save_list(SaveFieldsList),
	temporary_unsafe(TemporaryList),
	turning_back(IsTurning),
	NewArrows is Arrows + 1,
	forwardStep(X, Y, Orient,  Shooted_X, Shooted_Y),

	Action = shoot,
	moves(Move),
	AmountMoves is Move + 1,
	Knowledge = [gameStarted, wumpus_dead(IsDead), shooted_arrows(NewArrows), position_shooted(X, Y), moves(AmountMoves),
	             save_list(SaveFieldsList), temporary_unsafe(TemporaryList),turning_back(IsTurning),
		           haveGold(NGolds), myWorldSize(Max_X, Max_Y), myPosition(X, Y, Orient), myTrail(Trail)].

save_if_wumpus_dead(Action, Knowledge) :-
	scream,
	haveGold(NGolds),
	myWorldSize(Max_X, Max_Y),
	myPosition(X, Y, Orient),
	myTrail(Trail),
	wumpus_dead(IsDead),
	shooted_arrows(Arrows),
	position_shooted(Shooted_X, Shooted_Y),
  save_list(SaveFieldsList),
	temporary_unsafe(TemporaryList),
	turning_back(IsTurning),
	forwardStep(X, Y, Orient, New_X, New_Y),
	New_Trail = [ [Action,X,Y,Orient] | Trail ],

	Action = moveForward,
	moves(Move),
	AmountMoves is Move + 1,
	Knowledge = [gameStarted, wumpus_dead(1), shooted_arrows(Arrows), position_shooted(Shooted_X, Shooted_Y), moves(AmountMoves),
	 						 save_list(SaveFieldsList), temporary_unsafe(TemporaryList),turning_back(IsTurning),
							 haveGold(NGolds), myWorldSize(Max_X, Max_Y), myPosition(New_X, New_Y, Orient), myTrail(New_Trail)].




% go_if_stench(Action, Knowledge) :-
% 	stench,
% 	wumpus_dead(IsDead),
% 	shooted_arrows(Arrows),
% 	position_shooted(Shooted_X, Shooted_Y),
% 	myWorldSize(Max_X, Max_Y),
% 	myPosition(X, Y, Orient),
% 	myTrail(Trail),
% 	turning_back(IsTurning),
%
% 	wumpus_dead(IsDead), IsDead < 1,
% 	shooted_arrows(Arrows), Arrows > 0,
% 	Shooted_X = X, Shooted_Y = Y,
%   save_list(SaveFieldsList),
% 	temporary_unsafe(TemporaryList),
%
% 	%haveGold(NGolds), NGolds < 1,%tutaj jeszcze warunek ze jesli wumpus niezyje to zlewaj smierdziochy IsDead > 0,
%
%   Action = moveForward,
% 	moves(Move),
% 	AmountMoves is Move + 1,
% 	Knowledge = [gameStarted, shooted_arrows(Arrows), wumpus_dead(IsDead), position_shooted(Shooted_X, Shooted_Y), moves(AmountMoves),
% 							 save_list(SaveFieldsList), temporary_unsafe(TemporaryList),turning_back(IsTurning),
% 							 haveGold(NGolds), myWorldSize(Max_X, Max_Y), myPosition(X, Y, Orient), myTrail(Trail)].
% ====================================================================================================
exit_if_world_is_too_hard(Action, Knowledge) :-
	moves(Move), Move > 50,
	Action = grab,			    %this is easy, we are sitting on it
	haveGold(NGolds),		    %we must know how many golds we have
	NewNGolds is NGolds + 1,
	myWorldSize(Max_X, Max_Y),
	myPosition(X, Y, Orient),
	myTrail(Trail),
	wumpus_dead(IsDead),
	shooted_arrows(Arrows),
	position_shooted(Shooted_X, Shooted_Y),
	AmountMoves is Move + 1,
	New_Trail = [ [Action,X,Y,Orient] | Trail ], %important to remember grab
	Knowledge = [gameStarted,
				 wumpus_dead(IsDead),
				 shooted_arrows(Arrows),
				 moves(AmountMoves),
				 position_shooted(Shooted_X, Shooted_Y),
							 haveGold(NewNGolds),
				 myWorldSize(Max_X, Max_Y),
				 myPosition(X, Y, Orient),	%the position stays the same
				 myTrail(New_Trail)].


exit_if_home(Action, Knowledge) :-
	haveGold(NGolds), NGolds > 0,
	myPosition(1, 1, Orient),
	Action = exit,				%done game
	Knowledge = [].				%irrelevant but required

exit_if_breeze_on_start(Action, Knowledge) :-
	  breeze,
    myWorldSize(Max_X, Max_Y),
    myPosition(1, 1, Orient),

    Action = exit,
		moves(Move),
		AmountMoves is Move + 1,
    Knowledge = [].

go_back_step(Action, Knowledge) :-
	%%% assuming we have just found gold:
	%%% 1. our last action must have been grab
	%%% 2. our previuos action must have been moveForward
	%%% 3. so we are initiating a turnback and then return:
	%%%    (a) pop grab from the stack
	%%%    (b) replace it by an artificial turnRight we have never
	%%%        executed, but we will be reversing by turning left
	%%%    (c) execute a turnRight now which together will turn us back
	%%% 4. after that we are facing back and can execute actions in reverse
	%%% 5. because of grab we can be sure this rule is executed exactly once
	haveGold(NGolds), NGolds > 0,
	myWorldSize(Max_X, Max_Y),
	myTrail(Trail),
	wumpus_dead(IsDead),
	shooted_arrows(Arrows),
	position_shooted(Shooted_X, Shooted_Y),
	Trail = [ [grab,X,Y,Orient] | Trail_Tail ],
	New_Trail = [ [turnRight,X,Y,Orient] | Trail_Tail ], %Orient is misleading here
	Action = turnLeft,
	moves(Move),
	AmountMoves is Move + 1,
	Knowledge = [gameStarted,
				 wumpus_dead(IsDead),
				 moves(AmountMoves),
				 shooted_arrows(Arrows),
				 position_shooted(Shooted_X, Shooted_Y),
	             haveGold(NGolds),
		     myWorldSize(Max_X, Max_Y),
		     myPosition(X, Y, Orient),
		     myTrail(New_Trail)].

go_back_step(Action, Knowledge) :-
	wumpus_dead(IsDead),
	shooted_arrows(Arrows),
	position_shooted(Shooted_X, Shooted_Y),
	haveGold(NGolds), NGolds > 0,
	myWorldSize(Max_X, Max_Y),
	myTrail([ [Action,X,Y,Orient] | Trail_Tail ]),
	Action = moveForward,
	moves(Move),
	AmountMoves is Move + 1,
	Knowledge = [gameStarted,
				 wumpus_dead(IsDead),
				 shooted_arrows(Arrows),
				 moves(AmountMoves),
				 position_shooted(Shooted_X, Shooted_Y),
	             haveGold(NGolds),
		     myWorldSize(Max_X, Max_Y),
		     myPosition(X, Y, Orient),
		     myTrail(Trail_Tail)].

%%% backtracking a step can be moving or can be turning
go_back_step(Action, Knowledge) :- go_back_turn(Action, Knowledge).

go_back_turn(Action, Knowledge) :-
	wumpus_dead(IsDead),
	shooted_arrows(Arrows),
	position_shooted(Shooted_X, Shooted_Y),
	haveGold(NGolds), NGolds > 0,
	myWorldSize(Max_X, Max_Y),
	myTrail([ [OldAct,X,Y,Orient] | Trail_Tail ]),
	%% if our previous action was a turn, we must reverse it now
	((OldAct=turnLeft,Action=turnRight);(OldAct=turnRight,Action=turnLeft)),
	moves(Move),
	AmountMoves is Move + 1,
	Knowledge = [gameStarted,
				wumpus_dead(IsDead),
				shooted_arrows(Arrows),
				moves(AmountMoves),
				position_shooted(Shooted_X, Shooted_Y),
	             haveGold(NGolds),
		     myWorldSize(Max_X, Max_Y),
		     myPosition(X, Y, Orient),
		     myTrail(Trail_Tail)].

pick_up_gold(Action, Knowledge) :-
	glitter,
	Action = grab,			    %this is easy, we are sitting on it
	haveGold(NGolds),		    %we must know how many golds we have
	NewNGolds is NGolds + 1,
	myWorldSize(Max_X, Max_Y),
	myPosition(X, Y, Orient),
	myTrail(Trail),
	wumpus_dead(IsDead),
	shooted_arrows(Arrows),
	position_shooted(Shooted_X, Shooted_Y),
	moves(Move),
	AmountMoves is Move + 1,
	New_Trail = [ [Action,X,Y,Orient] | Trail ], %important to remember grab
	Knowledge = [gameStarted,
				 wumpus_dead(IsDead),
				 shooted_arrows(Arrows),
				 moves(AmountMoves),
				 position_shooted(Shooted_X, Shooted_Y),
	             haveGold(NewNGolds),
		     myWorldSize(Max_X, Max_Y),
		     myPosition(X, Y, Orient),	%the position stays the same
		     myTrail(New_Trail)].

turn_left_if_corner(Action, Knowledge) :-
	wumpus_dead(IsDead),
	shooted_arrows(Arrows),
	position_shooted(Shooted_X, Shooted_Y),
	myPosition(X, Y, Orient),
	save_list(SaveFieldsList),
	temporary_unsafe(TemporaryList),
	turning_back(IsBack),
	myWorldSize(Max_X,Max_Y),
	againstLeftWall(X, Y, Orient, Max_X, Max_Y),
	Action = turnLeft,			%always successful
	moves(Move),
	AmountMoves is Move + 1,
	shiftLeftOrient(Orient, NewOrient),		%always successful
	haveGold(NGolds),
	myTrail(Trail),
	New_Trail = [ [Action,X,Y,Orient] | Trail ],
	Knowledge = [gameStarted,
				 wumpus_dead(IsDead),
				 shooted_arrows(Arrows),
				 moves(AmountMoves),
		     haveGold(NGolds),
				 save_list(SaveFieldsList),
				 temporary_unsafe(TemporaryList),
				 turning_back(IsBack),
				 position_shooted(Shooted_X, Shooted_Y),
	             myWorldSize(Max_X, Max_Y),
		     myPosition(X, Y, NewOrient),
		     myTrail(New_Trail)].

 turn_right_if_corner(Action, Knowledge) :-
 	wumpus_dead(IsDead),
 	shooted_arrows(Arrows),
 	position_shooted(Shooted_X, Shooted_Y),
 	myPosition(X, Y, Orient),
 	save_list(SaveFieldsList),
 	temporary_unsafe(TemporaryList),
 	turning_back(IsBack),
 	myWorldSize(Max_X,Max_Y),
 	againstRightWall(X, Y, Orient, Max_X, Max_Y),
 	Action = turnRight,			%always successful
 	moves(Move),
 	AmountMoves is Move + 1,
 	shiftRightOrient(Orient, NewOrient),		%always successful
 	haveGold(NGolds),
 	myTrail(Trail),
 	New_Trail = [ [Action,X,Y,Orient] | Trail ],
 	Knowledge = [gameStarted,
 				 wumpus_dead(IsDead),
 				 shooted_arrows(Arrows),
 				 moves(AmountMoves),
 		     haveGold(NGolds),
 				 save_list(SaveFieldsList),
 				 temporary_unsafe(TemporaryList),
 				 turning_back(IsBack),
 				 position_shooted(Shooted_X, Shooted_Y),
 	             myWorldSize(Max_X, Max_Y),
 		     myPosition(X, Y, NewOrient),
 		     myTrail(New_Trail)].

 turn_if_wall(Action, Knowledge) :-
	rand_float(X), X<0.5,
 	wumpus_dead(IsDead),
 	shooted_arrows(Arrows),
 	position_shooted(Shooted_X, Shooted_Y),
 	myPosition(X, Y, Orient),
 	save_list(SaveFieldsList),
 	temporary_unsafe(TemporaryList),
 	turning_back(IsBack),
 	myWorldSize(Max_X,Max_Y),
 	againstWall(X, Y, Orient, Max_X, Max_Y),
 	Action = turnRight,			%always successful
 	moves(Move),
 	AmountMoves is Move + 1,
 	shiftRightOrient(Orient, NewOrient),		%always successful
 	haveGold(NGolds),
 	myTrail(Trail),
 	New_Trail = [ [Action,X,Y,Orient] | Trail ],
 	Knowledge = [gameStarted,
 				 wumpus_dead(IsDead),
 				 shooted_arrows(Arrows),
 				 moves(AmountMoves),
 		     haveGold(NGolds),
 				 save_list(SaveFieldsList),
 				 temporary_unsafe(TemporaryList),
 				 turning_back(IsBack),
 				 position_shooted(Shooted_X, Shooted_Y),
 	             myWorldSize(Max_X, Max_Y),
 		     myPosition(X, Y, NewOrient),
 		     myTrail(New_Trail)].

 turn_if_wall(Action, Knowledge) :-
 	wumpus_dead(IsDead),
 	shooted_arrows(Arrows),
 	position_shooted(Shooted_X, Shooted_Y),
 	myPosition(X, Y, Orient),
 	save_list(SaveFieldsList),
 	temporary_unsafe(TemporaryList),
 	turning_back(IsBack),
 	myWorldSize(Max_X,Max_Y),
 	againstWall(X, Y, Orient, Max_X, Max_Y),
 	Action = turnLeft,			%always successful
 	moves(Move),
 	AmountMoves is Move + 1,
 	shiftLeftOrient(Orient, NewOrient),		%always successful
 	haveGold(NGolds),
 	myTrail(Trail),
 	New_Trail = [ [Action,X,Y,Orient] | Trail ],
 	Knowledge = [gameStarted,
 				 wumpus_dead(IsDead),
 				 shooted_arrows(Arrows),
 				 moves(AmountMoves),
 		     haveGold(NGolds),
 				 save_list(SaveFieldsList),
 				 temporary_unsafe(TemporaryList),
 				 turning_back(IsBack),
 				 position_shooted(Shooted_X, Shooted_Y),
 	             myWorldSize(Max_X, Max_Y),
 		     myPosition(X, Y, NewOrient),
 		     myTrail(New_Trail)].

againstLeftWall(X, Y, Orient, Max_X, Max_Y) :- X = Max_X, Y = 1,     Orient = east.
againstLeftWall(X, Y, Orient, Max_X, Max_Y) :- X = Max_X, Y = Max_Y, Orient = north.
againstLeftWall(X, Y, Orient, Max_X, Max_Y) :- X = 1,     Y = Max_Y, Orient = west.
againstLeftWall(X, Y, Orient, Max_X, Max_Y) :- X = 1,     Y = 1,     Orient = south.

againstRightWall(X, Y, Orient, Max_X, Max_Y) :- X = Max_X, Y = 1,     Orient = south.
againstRightWall(X, Y, Orient, Max_X, Max_Y) :- X = Max_X, Y = Max_Y, Orient = east.
againstRightWall(X, Y, Orient, Max_X, Max_Y) :- X = 1,     Y = Max_Y, Orient = north.
againstRightWall(X, Y, Orient, Max_X, Max_Y) :- X = 1,     Y = 1,     Orient = west.

againstWall(X, Y, Orient, Max_X, Max_Y) :- X = Max_X, Orient = east.
againstWall(X, Y, Orient, Max_X, Max_Y) :- X = 1, Orient = west.
againstWall(X, Y, Orient, Max_X, Max_Y) :- Y = Max_Y, Orient = north.
againstWall(X, Y, Orient, Max_X, Max_Y) :- Y = 1, Orient = south.

shiftLeftOrient(east, north).
shiftLeftOrient(north, west).
shiftLeftOrient(west, south).
shiftLeftOrient(south, east).

shiftRightOrient(east, south).
shiftRightOrient(north, east).
shiftRightOrient(west, north).
shiftRightOrient(south, west).

else_move_on(Action, Knowledge) :-
	turning_back(IsTurning),IsTurning<1,
	rand_float(Rand), Rand<0.06,
	% not(breeze),
	Action = turnLeft,			%this will fail on a wall
	haveGold(NGolds),
	myWorldSize(Max_X,Max_Y),
	myPosition(X, Y, Orient),
	shiftLeftOrient(Orient, NewOrient),
	forwardStep(X, Y, Orient, New_X, New_Y),
	leftStep(X, Y, Orient, Left_X, Left_Y),
	rightStep(X, Y, Orient, Right_X, Right_Y),
	myTrail(Trail),
	wumpus_dead(IsDead),
	shooted_arrows(Arrows),
	position_shooted(Shooted_X, Shooted_Y),
	moves(Move),
	AmountMoves is Move + 1,
	save_list(SaveFieldsList),
	% add_to_save_list(New_X,New_Y,Left_X,Left_Y,Right_X,Right_Y,SaveFieldsList,NewSaveFieldsList),
	temporary_unsafe(TemporaryList),
	New_Trail = [ [Action,X,Y,Orient] | Trail ],
	Knowledge = [gameStarted,	wumpus_dead(IsDead), shooted_arrows(Arrows), position_shooted(Shooted_X, Shooted_Y), moves(AmountMoves),
							save_list(SaveFieldsList), temporary_unsafe(TemporaryList),turning_back(IsTurning),
		     haveGold(NGolds),
	       myWorldSize(Max_X, Max_Y),
		     myPosition(X, Y, NewOrient),
		     myTrail(New_Trail)].

else_move_on(Action, Knowledge) :-
	turning_back(IsTurning),IsTurning<1,
	rand_float(Rand), Rand<0.06,
	% not(breeze),
	Action = turnRight,			%this will fail on a wall
	haveGold(NGolds),
	myWorldSize(Max_X,Max_Y),
	myPosition(X, Y, Orient),
	shiftRightOrient(Orient, NewOrient),
	forwardStep(X, Y, Orient, New_X, New_Y),
	leftStep(X, Y, Orient, Left_X, Left_Y),
	rightStep(X, Y, Orient, Right_X, Right_Y),
	myTrail(Trail),
	wumpus_dead(IsDead),
	shooted_arrows(Arrows),
	position_shooted(Shooted_X, Shooted_Y),
	moves(Move),
	AmountMoves is Move + 1,
	save_list(SaveFieldsList),
	% add_to_save_list(New_X,New_Y,Left_X,Left_Y,Right_X,Right_Y,SaveFieldsList,NewSaveFieldsList),
	temporary_unsafe(TemporaryList),
	New_Trail = [ [Action,X,Y,Orient] | Trail ],
	Knowledge = [gameStarted,	wumpus_dead(IsDead), shooted_arrows(Arrows), position_shooted(Shooted_X, Shooted_Y), moves(AmountMoves),
							save_list(NewSaveFieldsList), temporary_unsafe(TemporaryList),turning_back(IsTurning),
		     haveGold(NGolds),
	       myWorldSize(Max_X, Max_Y),
		     myPosition(X, Y, NewOrient),
		     myTrail(New_Trail)].

else_move_on(Action, Knowledge) :-
	turning_back(IsTurning),IsTurning<1,
	% not(breeze),
	Action = moveForward,			%this will fail on a wall
	haveGold(NGolds),
	myWorldSize(Max_X,Max_Y),
	myPosition(X, Y, Orient),
	forwardStep(X, Y, Orient, New_X, New_Y),
	leftStep(X, Y, Orient, Left_X, Left_Y),
	rightStep(X, Y, Orient, Right_X, Right_Y),
	myTrail(Trail),
	wumpus_dead(IsDead),
	shooted_arrows(Arrows),
	position_shooted(Shooted_X, Shooted_Y),
	moves(Move),
	AmountMoves is Move + 1,
	save_list(SaveFieldsList),
	add_to_save_list(New_X,New_Y,Left_X,Left_Y,Right_X,Right_Y,SaveFieldsList,NewSaveFieldsList),
	temporary_unsafe(TemporaryList),
	New_Trail = [ [Action,X,Y,Orient] | Trail ],
	Knowledge = [gameStarted,	wumpus_dead(IsDead), shooted_arrows(Arrows), position_shooted(Shooted_X, Shooted_Y), moves(AmountMoves),
							save_list(NewSaveFieldsList), temporary_unsafe(TemporaryList),turning_back(IsTurning),
		     haveGold(NGolds),
	       myWorldSize(Max_X, Max_Y),
		     myPosition(New_X, New_Y, Orient),
		     myTrail(New_Trail)].

%%turn back if wumpus alive
 if_breeze_turn_back(Action, Knowledge) :-
 	turning_back(IsTurning),IsTurning<1,
 	stench,
	wumpus_dead(IsDead), IsDead > 0,
	shooted_arrows(Arrows), Arrows > 0,
 	haveGold(NGolds),
 	myWorldSize(Max_X,Max_Y),
 	myPosition(X, Y, Orient),
 	shiftLeftOrient(Orient, NewOrient),
 	forwardStep(X, Y, Orient, New_X, New_Y),
 	leftStep(X, Y, Orient, Left_X, Left_Y),
 	rightStep(X, Y, Orient, Right_X, Right_Y),
 	myTrail(Trail),
 	position_shooted(Shooted_X, Shooted_Y),
 	moves(Move),
 	AmountMoves is Move + 1,
 	save_list(SaveFieldsList),
 	temporary_unsafe(TemporaryList),
 	add_to_temporary_list(New_X,New_Y,Left_X,Left_Y,Right_X,Right_Y,TemporaryList,NewTemporaryList),

 	Action = turnLeft,

 	choose_way(Left_X,Left_Y,Right_X,Right_Y,Max_X,Max_Y,NewActions),
 	New_Trail = [ [Action,X,Y,Orient] | Trail ],
 	Knowledge = [gameStarted,	wumpus_dead(IsDead), shooted_arrows(Arrows), position_shooted(Shooted_X, Shooted_Y), moves(AmountMoves), turning_back(1),
 								back_actions(NewActions),save_list(SaveFieldsList),temporary_unsafe(NewTemporaryList),
 		     haveGold(NGolds),
 	       myWorldSize(Max_X, Max_Y),
 		     myPosition(X, Y, NewOrient),
 		     myTrail(New_Trail)].


if_breeze_turn_back(Action, Knowledge) :-
	turning_back(IsTurning),IsTurning<1,
	breeze,
	haveGold(NGolds),
	myWorldSize(Max_X,Max_Y),
	myPosition(X, Y, Orient),
	shiftLeftOrient(Orient, NewOrient),
	forwardStep(X, Y, Orient, New_X, New_Y),
	leftStep(X, Y, Orient, Left_X, Left_Y),
	rightStep(X, Y, Orient, Right_X, Right_Y),
	myTrail(Trail),
	wumpus_dead(IsDead),
	shooted_arrows(Arrows),
	position_shooted(Shooted_X, Shooted_Y),
	moves(Move),
	AmountMoves is Move + 1,
	save_list(SaveFieldsList),
	temporary_unsafe(TemporaryList),
	add_to_temporary_list(New_X,New_Y,Left_X,Left_Y,Right_X,Right_Y,TemporaryList,NewTemporaryList),

	Action = turnLeft,

	% NewActions = [[turnLeft],[moveForward],[turnRight],[moveForward],[turnRight]],
	choose_way(Left_X,Left_Y,Right_X,Right_Y,Max_X,Max_Y,NewActions),

	New_Trail = [ [Action,X,Y,Orient] | Trail ],
	Knowledge = [gameStarted,	wumpus_dead(IsDead), shooted_arrows(Arrows), position_shooted(Shooted_X, Shooted_Y), moves(AmountMoves), turning_back(1),
								back_actions(NewActions),save_list(SaveFieldsList),temporary_unsafe(NewTemporaryList),
		     haveGold(NGolds),
	       myWorldSize(Max_X, Max_Y),
		     myPosition(X, Y, NewOrient),
		     myTrail(New_Trail)].


	backing(Action, Knowledge) :-
		turning_back(IsTurning),IsTurning>0,
		haveGold(NGolds),
		myWorldSize(Max_X,Max_Y),
		myPosition(X, Y, Orient),
		forwardStep(X, Y, Orient, New_X, New_Y),
		leftStep(X, Y, Orient, Left_X, Left_Y),
		rightStep(X, Y, Orient, Right_X, Right_Y),
		myTrail(Trail),
		wumpus_dead(IsDead),
		shooted_arrows(Arrows),
		position_shooted(Shooted_X, Shooted_Y),
		moves(Move),
		AmountMoves is Move + 1,
		save_list(SaveFieldsList),
		temporary_unsafe(TemporaryList),

		back_actions([ [CurrentAction] | Actions_Tail]),
		%jesli lista jest pusta zmien isturning na 0
		% back_actions([]), IsTurning2=0,
		% ((back_actions([]),IsTurning=0);(not(back_actions([])),IsTurning=1)),
		% (back_actions([]) -> IsTurning=0;IsTurning=1),
		list_zerolength(Actions_Tail,IsTurning2),
		% ((Empty=true, IsTurning=0);(Empty = false, IsTurning=1)),
		CurrentAction=moveForward,
		Action = CurrentAction,
		New_Trail = [ [Action,X,Y,Orient] | Trail ],


		Knowledge = [gameStarted,	wumpus_dead(IsDead), shooted_arrows(Arrows), position_shooted(Shooted_X, Shooted_Y), moves(AmountMoves),
		 						turning_back(IsTurning2), back_actions(Actions_Tail),save_list(SaveFieldsList),temporary_unsafe(TemporaryList),
			     haveGold(NGolds),
		       myWorldSize(Max_X, Max_Y),
			     myPosition(New_X, New_Y, Orient),
			     myTrail(New_Trail)].


 backing(Action, Knowledge) :-
		turning_back(IsTurning),IsTurning>0,
		haveGold(NGolds),
		myWorldSize(Max_X,Max_Y),
		myPosition(X, Y, Orient),
		forwardStep(X, Y, Orient, New_X, New_Y),
		leftStep(X, Y, Orient, Left_X, Left_Y),
		rightStep(X, Y, Orient, Right_X, Right_Y),
		myTrail(Trail),
		wumpus_dead(IsDead),
		shooted_arrows(Arrows),
		position_shooted(Shooted_X, Shooted_Y),
		moves(Move),
		AmountMoves is Move + 1,
		save_list(SaveFieldsList),
		temporary_unsafe(TemporaryList),

		back_actions([ [CurrentAction] | Actions_Tail]),
		list_zerolength(Actions_Tail,IsTurning2),
		CurrentAction=turnLeft,
		shiftLeftOrient(Orient, NewOrient),
		Action = CurrentAction,
		New_Trail = [ [Action,X,Y,Orient] | Trail ],

		Knowledge = [gameStarted,	wumpus_dead(IsDead), shooted_arrows(Arrows), position_shooted(Shooted_X, Shooted_Y), moves(AmountMoves),
		 						turning_back(IsTurning2), back_actions(Actions_Tail),save_list(SaveFieldsList),temporary_unsafe(TemporaryList),
			     haveGold(NGolds),
		       myWorldSize(Max_X, Max_Y),
			     myPosition(X, Y, NewOrient),
			     myTrail(New_Trail)].

 backing(Action, Knowledge) :-
		turning_back(IsTurning),IsTurning>0,
		haveGold(NGolds),
		myWorldSize(Max_X,Max_Y),
		myPosition(X, Y, Orient),
		forwardStep(X, Y, Orient, New_X, New_Y),
		leftStep(X, Y, Orient, Left_X, Left_Y),
		rightStep(X, Y, Orient, Right_X, Right_Y),
		myTrail(Trail),
		wumpus_dead(IsDead),
		shooted_arrows(Arrows),
		position_shooted(Shooted_X, Shooted_Y),
		moves(Move),
		AmountMoves is Move + 1,
		save_list(SaveFieldsList),
		temporary_unsafe(TemporaryList),

		back_actions([ [CurrentAction] | Actions_Tail]),
		list_zerolength(Actions_Tail,IsTurning2),
		CurrentAction=turnRight,
		shiftRightOrient(Orient, NewOrient),
		Action = CurrentAction,
		New_Trail = [ [Action,X,Y,Orient] | Trail ],

		Knowledge = [gameStarted,	wumpus_dead(IsDead), shooted_arrows(Arrows), position_shooted(Shooted_X, Shooted_Y), moves(AmountMoves),
		 						turning_back(IsTurning2), back_actions(Actions_Tail),save_list(SaveFieldsList),temporary_unsafe(TemporaryList),
			     haveGold(NGolds),
		       myWorldSize(Max_X, Max_Y),
			     myPosition(X, Y, NewOrient),
			     myTrail(New_Trail)].


forwardStep(X, Y, east,  New_X, Y) :- New_X is (X+1).
forwardStep(X, Y, south, X, New_Y) :- New_Y is (Y-1).
forwardStep(X, Y, west,  New_X, Y) :- New_X is (X-1).
forwardStep(X, Y, north, X, New_Y) :- New_Y is (Y+1).

leftStep(X, Y, east,  X, New_Y) :- New_Y is (Y+1).
leftStep(X, Y, south, New_X, Y) :- New_X is (X+1).
leftStep(X, Y, west,  X, New_Y) :- New_Y is (Y-1).
leftStep(X, Y, north, New_X, Y) :- New_X is (X-1).

rightStep(X, Y, east,  X, New_Y) :- New_Y is (Y-1).
rightStep(X, Y, south, New_X, Y) :- New_X is (X-1).
rightStep(X, Y, west,  X, New_Y) :- New_Y is (Y+1).
rightStep(X, Y, north, New_X, Y) :- New_X is (X+1).

behindStep(X, Y, east,  New_X, Y) :- New_X is (X-1).
behindStep(X, Y, south, X, New_Y) :- New_Y is (Y+1).
behindStep(X, Y, west,  New_X, Y) :- New_X is (X+1).
behindStep(X, Y, north, X, New_Y) :- New_Y is (Y-1).

add_to_save_list(X1,Y1,X2,Y2,X3,Y3,OldList,NewList) :-
  ((member([X1,Y1], OldList),NewList1=OldList);(not(member([X1,Y1], OldList)),NewList1 = [ [X1,Y1] | OldList ])),
  ((member([X2,Y2], NewList1),NewList2=NewList1);(not(member([X2,Y2], NewList1))),NewList2 = [ [X2,Y2] | NewList1 ]),
  ((member([X3,Y3], NewList2),NewList=NewList2);(not(member([X3,Y3], NewList2))), NewList = [ [X3,Y3] | NewList2 ]).


add_to_temporary_list(X1,Y1,X2,Y2,X3,Y3,OldList,NewList) :-
	((member([X1,Y1], OldList),NewList1=OldList);(not(member([X1,Y1], OldList)),NewList1 = [ [X1,Y1] | OldList ])),
	((member([X2,Y2], NewList1),NewList2=NewList1);(not(member([X2,Y2], NewList1))),NewList2 = [ [X2,Y2] | NewList1 ]),
	((member([X3,Y3], NewList2),NewList=NewList2);(not(member([X3,Y3], NewList2))), NewList = [ [X3,Y3] | NewList2 ]).


list_zerolength(List, Empty) :-
    length(List, Len),
    (Len == 0 -> Empty is 0;Empty is 1).


% if wall is on right========================================
choose_way(Left_X,Left_Y,Right_X,Right_Y,Max_X,Max_Y,Way) :-
	Right_X>Max_X,
	Way=[[turnLeft],[moveForward],[turnRight],[moveForward]].

choose_way(Left_X,Left_Y,Right_X,Right_Y,Max_X,Max_Y,Way) :-
	Right_Y<1,
	Way=[[turnLeft],[moveForward],[turnRight],[moveForward]].

choose_way(Left_X,Left_Y,Right_X,Right_Y,Max_X,Max_Y,Way) :-
	Right_Y>Max_Y,
	Way=[[turnLeft],[moveForward],[turnRight],[moveForward]].

choose_way(Left_X,Left_Y,Right_X,Right_Y,Max_X,Max_Y,Way) :-
	Right_X<1,
	Way=[[turnLeft],[moveForward],[turnRight],[moveForward]].
%=if wall is on left=========================================
choose_way(Left_X,Left_Y,Right_X,Right_Y,Max_X,Max_Y,Way) :-
	Left_X>Max_X,
	Way=[[turnLeft],[moveForward],[turnLeft],[moveForward]].

choose_way(Left_X,Left_Y,Right_X,Right_Y,Max_X,Max_Y,Way) :-
	Left_Y<1,
	Way=[[turnLeft],[moveForward],[turnLeft],[moveForward]].

choose_way(Left_X,Left_Y,Right_X,Right_Y,Max_X,Max_Y,Way) :-
	Left_Y>Max_Y,
	Way=[[turnLeft],[moveForward],[turnLeft],[moveForward]].

choose_way(Left_X,Left_Y,Right_X,Right_Y,Max_X,Max_Y,Way) :-
	Left_X<1,
	Way=[[turnLeft],[moveForward],[turnLeft],[moveForward]]. %,[turnRight]
%=in centre of the board================================================
choose_way(Left_X,Left_Y,Right_X,Right_Y,Max_X,Max_Y,Way) :-
	rand_float(X), X<0.4,
	Way=[[turnLeft],[moveForward],[turnRight],[moveForward]]. %[turnLeft]

choose_way(Left_X,Left_Y,Right_X,Right_Y,Max_X,Max_Y,Way) :-
	rand_float(X), X<0.4,
	Way=[[turnLeft],[moveForward],[turnLeft],[moveForward]]. %,[turnRight]
%=otherwise============================================================
choose_way(Left_X,Left_Y,Right_X,Right_Y,Max_X,Max_Y,Way) :-
	Way=[[turnLeft],[moveForward]]. %,[turnRight],[moveForward],[turnLeft],[turnLeft]


% wprowadzic jeszce randomowe ruchy podczas ruchu do przodu - jest
% przy omijaniu przeszkody przy scianie zdaza sie ze wpadnie do dziury
% zabezpieczyc jesli wumpus zyje
%			- jesli smierdzi na 1,1 strzelic i idz
%			- dodac liste smierdzacych pol i potem wyliczac pozycje
% dodac wychodzenie jesli po 50 ruchach nie znajdzie zlota - jest
% dodac obcinanie w my_trail ruchow z omijania
