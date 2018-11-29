/* For SWI prolog compatibility (for debugging purposes) */
/*
:- dynamic breeze/2, glitter/2, stench/2, pit/2, wumpus/2, gold/2, pit/2,
randomize/0, exit/2, player/3, bump/0, scream/0.
% */

/*
  Wumpus World, based on
  Artifical Intelligence, A Modern Approach, Second Edition
  Chapter 7.2: The Wumpus World

  -----------------------------------------------------------------------------

  Interface:
  Action from agent's act(Action,Knowledge) must unify to one of
  actions described below.

  PlayerPercepts from
	generate_world(GeneratedWorld, PlayerKnowledge, PlayerPercepts)
  and
	react(GameStatus, NextWorld, PlayerPercepts)
  unify to list of percepts, where every percepts must be one of
  sensors described below.

  For details of simulation protocol see ./worlds/world_template.pl

  -----------------------------------------------------------------------------
  Performance measure:
  -----------------------------------------------------------------------------

  Falling into pit:			-1000
  Being eaten by Wumpus:		-1000
  Shooting arrow:			  -10
  Taking action:			   -1
  Initial:				    0
  Killing Wumpus:			    0
  Picking up gold:			+1000

  -----------------------------------------------------------------------------
  Environment:
  -----------------------------------------------------------------------------

  - world can by of any size X,Y where and X,Y>=1;
  - there is exactly 1 agent;
  - agent starts in cell (1,1), facing east;
  - there is exactly one wumpus;
  - there is exactly one gold;
  - there can be any number of pits, from 0 to X*Y-1 (-1 for agent position);
  - probability of pit occuring in any given cell is 0.2. Exception to this
    is cell with agent (1,1), which will never have a pit;
  - agent won't immediately lose, i.e. won't start game on pit or wumpus;
  - cell beyond X,Y boundaries contain wall;
  - if agent enters cell with alive wumpus or pit, agent dies;
  - cell with dead wumpus is safe;
  - wumpus doesn't fall into the pit - he is too fat from eating previous
    adventurers;

  ----------------------------------------------------------------------------
  Actuators:
  ----------------------------------------------------------------------------

  moveForward	- has no effect if there is a wall in front of the agent;
  turnLeft	- turn left 90 degrees;
  turnRight	- turn right 90 degrees;
  shoot		- agent shoots arrow in direction she is facing.
    Shot arrow flies by cells in straight line until it hits a wall or wumpus.
    If wumpus is hit, it dies, and cell containing it becomes safe to enter.
    Agent has only one arrow, so only the first shoot action has any effect.
  grab		- agent can grab a gold if stands in a cell containing it.
  exit		- agent can exit cave if she is in a starting cell (1,1).

  ----------------------------------------------------------------------------
  Sensors:
  ----------------------------------------------------------------------------

  stench	- if agent is adjacent to Wumpus or stands on a dead wumpus;
  breeze	- if agent is adjacent to pit;
  glitter	- if agent is in a cell with a gold;
  bump		- if agent tried to walk into a wall;
  scream	- if agent killed wumpus by shooting arrow and hitting him.

  worldSize(X,Y) - special sensor available in agent knowledge base only
  at first call to act/2. X and Y are integers denoting world size.

  Note:
  cells are "adjacent" if they differ in exactly one coordinate by 1, in
  particular, cells placed diagonally are NOT adjacent.

*/

/* -------------------------------------------------------------------------- */
/* Default world parameters */
/* -------------------------------------------------------------------------- */

default_world_params(Params) :-
% Randomization
	Params = [
		randomize,
		worldSize(7,7),
		player(1,1,east),
		arrows(1)
	].
% */
% Using presets
/*
	Params = [preset(1)]. % Available presets: 1-7
% */
/*
 Params = [
		comment('To see how to randomize world or use preset worlds, \
see worlds/wumpus_world.pl'),

		worldSize(7,7),
		player(4,4, north),
		arrows(3),

		pit(2,2),
		pit(3,5),
		pit(5,2),
		pit(7,6),
		pit(7,7),

		wumpus(2,3),
		wumpus(2,6),
		wumpus(5,2),
		wumpus(5,5),
		wumpus(7,7),

		gold(1,1),
		gold(1,7),
		gold(2,3),
		gold(3,5),
		gold(7,1),
		gold(7,7)
	].
% */

/* -------------------------------------------------------------------------- */
/* Initial world generation: presets */
/* -------------------------------------------------------------------------- */

% przyklad duzy i lamiacy pewne podstawowe zalozenia
getPreset(1,
		worldSize(7,7),
		player(4,4, north),
		arrows(3),
		[
		pit(2,2),
		pit(3,5),
		pit(5,2),
		pit(7,6),
		pit(7,7)
		],[
		wumpus(2,3),
		wumpus(2,6),
		wumpus(5,2),
		wumpus(5,5),
		wumpus(7,7)
		],[
		gold(1,1),
		gold(1,7),
		gold(2,3),
		gold(3,5),
		gold(7,1),
		gold(7,7)
	        ]
).

%% przyk�ad z wykladu (+Russell&Norvig)
getPreset(2,
		worldSize(4,4),
		player(1,1,east),
		arrows(1),
		[
			pit(3,1),
			pit(3,3),
			pit(4,4)
		],
		[
			wumpus(1,2)
		],
		[
			gold(2,3)
		]
).

getPreset(3,

		worldSize(3,3),
		player(1,1,east),
		arrows(1),
		[],
		[
			wumpus(2,2)
		],
		[
			gold(3,3)
		]
).

getPreset(4,

		worldSize(3,3),
		player(1,1,east),
		arrows(1),
		[
			pit(3,1),
			pit(3,3)
		],
		[
			wumpus(3,2)
		],
		[
			gold(3,2)
		]
).

getPreset(5,

		worldSize(3,4),
		player(1,1,east),
		arrows(1),
		[
			pit(1,4),
			pit(3,1)
		],
		[
			wumpus(3,4)
		],
		[
			gold(2,4)
		]
).

getPreset(6,

		worldSize(6,5),
		player(1,1,east),
		arrows(1),
		[
			pit(2,3),
			pit(4,3)
		],
		[
			wumpus(3,3)
		],
		[
			gold(3,3)
		]
).

getPreset(7,

		worldSize(6,6),
		player(1,1,east),
		arrows(1),
		[
			pit(2,3),
			pit(5,5),
			pit(1,6),
			pit(5,1)
		],
		[
			wumpus(5,3)
		],
		[
			gold(3,6)
		]
).

getPreset(8,

		worldSize(5,5),
		player(1,1,east),
		arrows(1),
		[
			pit(2,3),
			pit(5,5),
			pit(1,6),
			pit(5,1)
		],
		[
			wumpus(2,1)
		],
		[
			gold(3,6)
		]
).

getPreset(9,

		worldSize(5,5),
		player(1,1,east),
		arrows(0),
		[
		],
		[
		],
		[
		]
).

getPreset(11,

		worldSize(7,7),
		player(1,1,east),
		arrows(1),
		[
		  pit(3,1),
			pit(3,3),
			pit(6,4)
		],
		[
			wumpus(6,5)
		],
		[
			gold(2,2)
		]
).

/* -------------------------------------------------------------------------- */
/* Initial world generation */
/* -------------------------------------------------------------------------- */

/* Randomized world generation */
generate_world(GeneratedWorld, PlayerKnowledge, PlayerPercepts) :-

	% Check world generation type
	randomize,

	% Bindings
	worldSize(X,Y),

	randomizeCoor(X,Y,Gx,Gy),
	randomizeCoor(X,Y,Wx,Wy),
	randomizePits(X,Y,Pits),

	constructWorld(
		worldSize(X,Y), player(1,1,east), arrows(1),
		Pits, [wumpus(Wx,Wy)], [gold(Gx,Gy)],
		GeneratedWorld, PlayerKnowledge, PlayerPercepts
	).

/* Preset world generation */
generate_world(GeneratedWorld, PlayerKnowledge, PlayerPercepts) :-

	% Bind preset
	preset(PresetNo),

	getPreset(PresetNo,
		worldSize(X,Y),
		player(Px,Py,PFDir),
		arrows(ArrowsC),
		Pits,
		Wumpi,
		Golds
	),

	constructWorld(
		worldSize(X,Y), player(Px,Py,PFDir), arrows(ArrowsC),
		Pits, Wumpi, Golds,
		GeneratedWorld, PlayerKnowledge, PlayerPercepts
	).

/* Configured world generation */
generate_world(GeneratedWorld, PlayerKnowledge, PlayerPercepts) :-

	% Bindings
	worldSize(X,Y), player(Px,Py,PFDir), arrows(ArrowsC),

	gatherPits(Pits),
	gatherWumpi(Wumpi),
	gatherGolds(Golds),

	constructWorld(
		worldSize(X,Y), player(Px,Py,PFDir), arrows(ArrowsC),
		Pits, Wumpi, Golds,
		GeneratedWorld, PlayerKnowledge, PlayerPercepts
	).


constructWorld(
			worldSize(X,Y), player(Px,Py,PFDir), arrows(ArrowsC),
			Pits, Wumpi, Golds,
/* OUT: */	GeneratedWorld, PlayerKnowledge, PlayerPercepts) :-

	generateStenches(X,Y,Wumpi,Stenches),
	generateBreezes(X,Y,Pits,Breezes),

	append(
		[
			[
				player(Px,Py,PFDir),
				exit(Px,Py),
				worldSize(X,Y),
				arrows(ArrowsC),
				initArrows(ArrowsC),
				moveCounter(0),
				goldPickedUpCounter(0),
				wumpiKilledCounter(0)
			],
			Stenches,
			Breezes,
			Pits,
			Wumpi,
			Golds
		],
		GeneratedWorld
	),

	PlayerKnowledge = [worldSize(X,Y)],
	sensoryDataAt((Px,Py), Breezes, Stenches, Golds, PlayerPercepts).

/* generate_world -> randomizations */

randomizeCoor(X,Y,Rx,Ry) :-
	rand_int(X,RxDec), rand_int(Y,RyDec),
	Rx is RxDec + 1,
	Ry is RyDec + 1,
	(Rx \= 1; Ry \= 1).
randomizeCoor(X,Y,Rx,Ry) :-
	randomizeCoor(X,Y,Rx,Ry).

randomizePits(X,Y,Pits) :-
	generateCoors(X,Y,Coors),
	randomizePits(Coors,Pits).

generateCoors(X,Y,Coors) :-
	range(1,X,XRange),
	range(1,Y,YRange),
	listProduct(XRange,YRange,RCoors),
	delete(RCoors,[1,1],Coors).

randomizePits(Coors,Pits):-
	accRandomizePits(Coors,[],Pits).
accRandomizePits([],Acc,Acc).
accRandomizePits([[X,Y]|T],Acc,Pits) :-
	rand_float(Flt), Flt =< 0.1,
	accRandomizePits(T,[pit(X,Y)|Acc],Pits).
accRandomizePits([_|T],Acc,Pits) :-
	accRandomizePits(T,Acc,Pits).

/* generate_world -> ... */

generateStenches(X,Y,Wumpi,Stenches) :-
	accGenerateStenches(X,Y,Wumpi,[],Stenches).
accGenerateStenches(_,_,[],Acc,Acc).
accGenerateStenches(WrldX,WrldY,[wumpus(Wx,Wy)|Wumpi], Acc, Stenches) :-
	setof(stench(X,Y),adjacent(WrldX,WrldY,Wx,Wy,X,Y),AdjStenches),
	append(AdjStenches,[stench(Wx,Wy)], CurrStenches),
	append(Acc,CurrStenches,NewAcc),
	accGenerateStenches(WrldX,WrldY,Wumpi,NewAcc,Stenches).

generateBreezes(X,Y,Pits,Breezes) :-
	accGenerateBreezes(X,Y,Pits,[],Breezes).
accGenerateBreezes(_,_,[],Acc,Acc).
accGenerateBreezes(WrldX,WrldY,[pit(Ix,Iy)|Pits], Acc, Breezes) :-
	setof(breeze(X,Y),adjacent(WrldX,WrldY, Ix,Iy,X,Y), CurrBreezes),
	append(Acc,CurrBreezes,NewAcc),
	accGenerateBreezes(WrldX,WrldY,Pits,NewAcc,Breezes).

sensoryDataAt((Px,Py), Breezes, Stenches, Golds, SensoryData) :-
	glitterAt((Px,Py), Golds, Glitter),
	breezeAt((Px,Py), Breezes, Breeze),
	stenchAt((Px,Py), Stenches, Stench),
	append([Glitter, Breeze, Stench],SensoryData).

/* generate_world -> sensoryDataAt -> ... */

glitterAt((X,Y), [gold(X,Y)|_], [glitter]).
glitterAt((X,Y), [_|T], Glitter) :-
	glitterAt((X,Y), T, Glitter).
glitterAt(_, [], []).

breezeAt((X,Y), [breeze(X,Y)|_], [breeze]).
breezeAt((X,Y), [_|T], Breeze) :-
	breezeAt((X,Y), T, Breeze).
breezeAt(_, [], []).

stenchAt((X,Y), [stench(X,Y)|_], [stench]).
stenchAt((X,Y), [_|T], Stench) :-
	breezeAt((X,Y), T, Stench).
stenchAt(_, [], []).

/* -------------------------------------------------------------------------- */
/* World display generation */
/* -------------------------------------------------------------------------- */

generate_display(DisplaySize,Display) :-

	% Bindings
	worldSize(Wx,Wy),

	DisplaySize = (Wx,Wy),

	displayPlayer(PlayerDisplay),
	displayExit(ExitDisplay),

	gatherDWumpiPitsGolds(WumpiPitsGoldsDisplay),
	gatherDWumpiGolds(WumpiGoldsDisplay),
	gatherDWumpiPits(WumpiPitsDisplay),
	gatherDWumpi(WumpiDisplay),
	gatherDWumpiDying(WumpiDyingDisplay),
	gatherDPitsGolds(PitsGoldsDisplay),
	gatherDGolds(GoldsDisplay),
	gatherDPits(PitsDisplay),
	gatherDStenches(StenchesDisplay),
	gatherDBreezes(BreezesDisplay),
	gatherDStenchesBreezes(StenchesBreezesDisplay),

	append(
		[
			PlayerDisplay,
			ExitDisplay,
			WumpiPitsGoldsDisplay,
			WumpiGoldsDisplay,
			WumpiPitsDisplay,
			WumpiDisplay,
			WumpiDyingDisplay,
			PitsGoldsDisplay,
			GoldsDisplay,
			PitsDisplay,
			StenchesDisplay,
			BreezesDisplay,
			StenchesBreezesDisplay
		],
		Display
	).

/* generate_display -> ... */

gatherDWumpiPitsGolds(WumpiPitsGoldsDisplay) :-
	setof(
		(X,Y,Img),
		displayWumpusPitGold((X,Y,Img)),
		WumpiPitsGoldsDisplay
	).
gatherDWumpiPitsGolds([]).


gatherDWumpiGolds(WumpiGoldsDisplay) :-
	setof(
		(X,Y,Img),
		displayWumpusGold((X,Y,Img)),
		WumpiGoldsDisplay
	).
gatherDWumpiGolds([]).

gatherDWumpiPits(WumpiPitsDisplay) :-
	setof(
		(X,Y,Img),
		displayWumpusPit((X,Y,Img)),
		WumpiPitsDisplay
	).
gatherDWumpiPits([]).

gatherDWumpi(WumpiDisplay) :-
	setof(
		(X,Y,Img),
		displayWumpus((X,Y,Img)),
		WumpiDisplay
	).
gatherDWumpi([]).

gatherDWumpiDying(WumpiDyingDisplay) :-
	setof(
		(X,Y,Img),
		displayWumpusDying((X,Y,Img)),
		WumpiDyingDisplay
	).
gatherDWumpiDying([]).

gatherDPitsGolds(PitsGoldsDisplay) :-
	setof(
		(X,Y,Img),
		displayPitGold((X,Y,Img)),
		PitsGoldsDisplay
	).
gatherDPitsGolds([]).

gatherDGolds(GoldsDisplay) :-
	setof(
		(X,Y,Img),
		displayGold((X,Y,Img)),
		GoldsDisplay
	).
gatherDGolds([]).

gatherDPits(PitsDisplay) :-
	setof(
		(X,Y,Img),
		displayPit((X,Y,Img)),
		PitsDisplay
	).
gatherDPits([]).

gatherDStenches(StenchesDisplay) :-
	setof(
		(X,Y,Img),
		displayStench((X,Y,Img)),
		StenchesDisplay
	).
gatherDStenches([]).

gatherDBreezes(BreezesDisplay) :-
	setof(
		(X,Y,Img),
		displayBreeze((X,Y,Img)),
		BreezesDisplay
	).
gatherDBreezes([]).

gatherDStenchesBreezes(StenchesBreezesDisplay) :-
	setof(
		(X,Y,Img),
		displayStenchBreeze((X,Y,Img)),
		StenchesBreezesDisplay
	).
gatherDStenchesBreezes([]).

displayPlayer([(X,Y,Img)]) :-
	player_shot(X,Y,SDir),
	displayShootDir(SDir, Img).

displayPlayer([(X,Y,Img)]) :-
	player(X,Y,PFDir),
	not(wumpus(X,Y)),
	not(pit(X,Y)),
	displayPlayerDir(PFDir,Img).
displayPlayer([]).

displayShootDir(north, 'shoot_N.bmp').
displayShootDir(east , 'shoot_E.bmp').
displayShootDir(west , 'shoot_W.bmp').
displayShootDir(south, 'shoot_S.bmp').

displayPlayerDir(north, 'player_N.bmp').
displayPlayerDir(east , 'player_E.bmp').
displayPlayerDir(west , 'player_W.bmp').
displayPlayerDir(south, 'player_S.bmp').

displayWumpusPitGold((X,Y,'wumpus_pit_gold.bmp')) :-
	wumpus(X,Y),
	pit(X,Y),
	gold(X,Y).

displayWumpusPit((X,Y,'wumpus_pit.bmp')) :-
	wumpus(X,Y),
	not(gold(X,Y)),
	pit(X,Y).

displayWumpusGold((X,Y,'wumpus_gold.bmp')) :-
	wumpus(X,Y),
	gold(X,Y),
	not(pit(X,Y)).

displayWumpus((X,Y,'wumpus.bmp')) :-
	wumpus(X,Y),
	not(gold(X,Y)),
	not(pit(X,Y)).

displayWumpusDying((X,Y,'wumpus_dying.bmp')) :- wumpus_dying(X,Y).

displayStenchBreeze((X,Y,'stenchbreeze.bmp')) :-
	stench(X,Y), breeze(X,Y),
	not(pit(X,Y)),
	not(wumpus(X,Y)),
	not(gold(X,Y)),
	not(exit(X,Y)),
	not(player(X,Y,_)),
	not(player_shot(X,Y,_)),
	not(wumpus_dying(X,Y)).


displayStench((X,Y,'stench.bmp')) :-
	stench(X,Y),
	not(breeze(X,Y)),
	not(pit(X,Y)),
	not(wumpus(X,Y)),
	not(gold(X,Y)),
	not(exit(X,Y)),
	not(player(X,Y,_)),
	not(player_shot(X,Y,_)),
	not(wumpus_dying(X,Y)).

displayBreeze((X,Y,'breeze.bmp')) :-
	breeze(X,Y),
	not(stench(X,Y)),
	not(pit(X,Y)),
	not(wumpus(X,Y)),
	not(gold(X,Y)),
	not(exit(X,Y)),
	not(player(X,Y,_)),
	not(player_shot(X,Y,_)),
	not(wumpus_dying(X,Y)).

displayPitGold((X,Y,'pit_gold.bmp')) :-
	pit(X,Y), gold(X,Y),
	not(wumpus(X,Y)),
	not(wumpus_dying(X,Y)).

displayPit((X,Y,'pit.bmp')) :-
	pit(X,Y),
	not(gold(X,Y)),
	not(wumpus(X,Y)),
	not(wumpus_dying(X,Y)).

displayGold((X,Y,'gold.bmp')) :-
	gold(X,Y),
	not(pit(X,Y)),
	not(player(X,Y,_)),
	not(wumpus(X,Y)),
	not(player_shot(X,Y,_)).

displayExit([(X,Y,'exit.bmp')]) :-
	exit(X,Y),
	not(player(X,Y,_)),
	not(player_shot(X,Y,_)).
displayExit([]).

/* -------------------------------------------------------------------------- */
/* Current player points retrieval */
/* -------------------------------------------------------------------------- */

points(Points) :-
	pointsWumpus(PW),
	pointsGold(PG),
	pointsArrow(PA),
	pointsDeath(PD),
	pointsMove(PM),
	Points is PW + PG + PA + PD + PM.

pointsWumpus(Points) :-
	wumpiKilledCounter(C),
	Points is 0*C.

pointsGold(Points) :-
	goldPickedUpCounter(C),
	Points is 1000*C.

pointsArrow(Points) :-
	initArrows(IC),
	arrows(C),
	Points is -10*(IC-C).

pointsDeath(-1000) :-
	player(X,Y,_), wumpus(X,Y);
	player(X,Y,_), pit(X,Y).
pointsDeath(0).

pointsMove(P) :-
	moveCounter(C),
	P is -1*C.

/* -------------------------------------------------------------------------- */
/* World iteration */
/* -------------------------------------------------------------------------- */

react(GameStatus, NextWorld, PlayerPercepts) :-

	% Bind predicates generated by player
	player_action(Action),

	% Remove temporary predicates.
	removeTemp,

	% Modify knowledge base according to action and retrieve status
	executeAction(Action, Status),

	/* --- ASSERT --- */
	/* Knowledge base modified accordingly to action. Possible modifications:
	- player/3 coordinates changed or player retracted (by exiting).
	- percepts bump/0 or scream/0 added;
	- gold/2 or wumpus/2 retracted;
	- arrows/1 count reduced by 1.
	*/

	makeGameStatus(Status, GameStatus),

	makeNextWorld(NextWorld),

	percept(PlayerPercepts).

removeTemp :-
	tryRemoveWumpusDying,
	tryRemovePlayerShot.

tryRemoveWumpusDying :-
	setof(_,retract(wumpus_dying(_,_)),_).
tryRemoveWumpusDying.

tryRemovePlayerShot :-
	retract(player_shot(_,_,_)).
tryRemovePlayerShot.

/* -------------------------------------------------------------------------- */
/* World iteration: executing action */
/* -------------------------------------------------------------------------- */

executeAction(Action, Status) :-
	Action = turnLeft,		turnLeftAction(Status)	;
	Action = turnRight,		turnRightAction(Status)	;
	Action = moveForward,	forwardAction(Status)	;
	Action = shoot,			shootAction(Status)		;
	Action = grab,			grabAction(Status)		;
	Action = exit,			exitAction(Status)		.

/* -------------------------------------------------------------------------- */
/* Action: Turning */

turnLeftAction([]) :-
	retract(player(X, Y, FacingDir)),
	turnLeft(FacingDir, NewFacingDir),
	asserta(player(X, Y, NewFacingDir)).
turnLeft(north , west ).
turnLeft(east  , north).
turnLeft(south , east ).
turnLeft(west  , south).

turnRightAction([]) :-
	retract(player(X, Y, FacingDir)),
	turnRight(FacingDir, NewFacingDir),
	asserta(player(X, Y, NewFacingDir)).
turnRight(north, east ).
turnRight(east , south).
turnRight(south, west ).
turnRight(west , north).

/* -------------------------------------------------------------------------- */
/* Action: Moving forward */

forwardAction(Status) :-
	% Bind player
	player(Px, Py, FacingDir),
	% compute next position taking bumps into consideration
	tryMoveForward(Px, Py, FacingDir, NewPx, NewPy, Status),
	retract(player(Px, Py, FacingDir)),
	asserta(player(NewPx, NewPy, FacingDir)).

tryMoveForward(Px, Py, FacingDir, NewPx, NewPy, Status) :-

	moveForward(Px, Py, FacingDir, NewPx, NewPy),

	inBounds(NewPx, NewPy),
	playerEatenByWumpus(NewPx, NewPy, WStatus),
	playerFellIntoPit(NewPx, NewPy, PStatus),

	append(WStatus, PStatus, Status).

tryMoveForward(Px, Py, _, Px, Py, [playerBumped]) :-
	asserta(bump). % For percepting

moveForward(PX,PY, Direction, NewPX,NewPY) :-
	Direction = north, NewPX =  PX,   NewPY is PY+1;
	Direction = west,  NewPX is PX-1, NewPY = PY;
	Direction = east,  NewPX is PX+1, NewPY = PY;
	Direction = south, NewPX =  PX,	  NewPY is PY-1.

inBounds(X, Y) :-
	worldSize(Wx,Wy), % Bind world size
	1 =< X, X =< Wx,
	1 =< Y, Y =< Wy.

playerEatenByWumpus(X,Y,[eatenByWumpus]) :-
	wumpus(X,Y).
playerEatenByWumpus(_,_,[]).

playerFellIntoPit(X,Y,[fallenIntoPit]) :-
	pit(X,Y).
playerFellIntoPit(_,_,[]).

/* -------------------------------------------------------------------------- */
/* Action: Shooting */

shootAction([arrowsDepleted]) :-
	arrows(0).

shootAction(Status) :-

	% Bindings
	arrows(Count), player(Px,Py, Direction),

	NewCount is Count - 1,
	retract(arrows(Count)),
	asserta(arrows(NewCount)),

	fireArrow(Direction, Px, Py, Status).

fireArrow(Direction, Px, Py, [wumpusKilled]) :-
	setof(_,fireAtWumpus(Direction,Px,Py),_),
	asserta(player_shot(Px,Py,Direction)). % for displaying

fireArrow(Direction, Px, Py, [wumpusKilled]) :-
	asserta(player_shot(Px,Py,Direction)). % for displaying

fireAtWumpus(Direction, Px, Py) :-
	wumpus(Wx, Wy), % Bind wumpus
	wumpusHit(Direction, Px, Py, Wx, Wy),

	wumpiKilledCounter(C),
	Cpp is C+1,
	retract(wumpiKilledCounter(C)),
	asserta(wumpiKilledCounter(Cpp)),

	asserta(scream), % for percepting
	asserta(wumpus_dying(Wx,Wy)), % for displaying
	retract(wumpus(Wx,Wy)).

wumpusHit(north,X,Py,X,Wy) :- Py < Wy.
wumpusHit(south,X,Py,X,Wy) :- Py > Wy.
wumpusHit(east,Px,Y,Wx,Y)  :- Px < Wx.
wumpusHit(west,Px,Y,Wx,Y)  :- Px > Wx.

/* -------------------------------------------------------------------------- */
/* Other actions */

exitAction([exited]) :-
	player(X,Y,_), exit(X,Y),
	retract(player(X,Y,_)).
exitAction([noExit]).

grabAction([goldPickedUp]) :-

	% Bindings
	player(X,Y,_), gold(X,Y),

	retract(gold(X,Y)),

	goldPickedUpCounter(C), % Bind C
	Cpp is C+1,
	retract(goldPickedUpCounter(C)),
	asserta(goldPickedUpCounter(Cpp)).

grabAction([nothingToGrab]).

/* -------------------------------------------------------------------------- */
/* World iteration: making game status, next world and player percepts */
/* -------------------------------------------------------------------------- */
/* Making game status */

makeGameStatus(Status,end('Player was eaten by wumpus!')) :-
	member(eatenByWumpus,Status).
makeGameStatus(Status,end('Player fell into the pit!')) :-
	member(fallenIntoPit,Status).

makeGameStatus(Status,end('Player exited.')) :-
	member(exited,Status).
makeGameStatus(Status,continue('Player tried to exit at invalid location!')) :-
	member(noExit,Status).

makeGameStatus(Status,continue('Player killed wumpus!')) :-
	member(wumpusKilled,Status).
makeGameStatus(Status,continue('Player tried to shoot but has no arrows!')) :-
	member(arrowsDepleted,Status).


makeGameStatus(Status,continue('Player picked up the gold.')) :-
	member(goldPickedUp,Status).
makeGameStatus(Status,continue('Player tried to grab nothing!')) :-
	member(nothingToGrab,Status).

makeGameStatus(_,continue('')).


/* -------------------------------------------------------------------------- */
/* Making next world */

makeNextWorld(NextWorld) :-

	% Bindings
	initArrows(IArrowsC), arrows(ArrowsC),
	worldSize(WrldX,WrldY), exit(ExitX,ExitY),
	goldPickedUpCounter(GoldC),
	wumpiKilledCounter(WumpC),
	moveCounter(MoveC),
	MoveCpp is MoveC+1,

	gatherStenches(Stenches),
	gatherBreezes(Breezes),
	gatherPits(Pits),
	gatherGolds(Golds),
	gatherWumpi(Wumpi),
	gatherDyingWumpi(DyingWumpi),

	getPlayer(Player),
	getPlayerShot(PlayerShot),

	append(
		[
			[
				initArrows(IArrowsC),
				arrows(ArrowsC),
				worldSize(WrldX,WrldY),
				exit(ExitX,ExitY),
				goldPickedUpCounter(GoldC),
				wumpiKilledCounter(WumpC),
				moveCounter(MoveCpp)
			],
			Stenches,
			Breezes,
			Pits,
			Player,
			PlayerShot,
			Wumpi,
			DyingWumpi,
			Golds
		],
		NextWorld).

gatherStenches(Stenches) :-
	setof(stench(Sx,Sy),stench(Sx,Sy),Stenches).
gatherStenches([]).

gatherBreezes(Breezes) :-
	setof(breeze(Bx,By),breeze(Bx,By),Breezes).
gatherBreezes([]).

gatherPits(Pits) :-
	setof(pit(Ix,Iy),pit(Ix,Iy),Pits).
gatherPits([]).

gatherWumpi(Wumpi) :-
	setof(wumpus(Wx,Wy),wumpus(Wx,Wy),Wumpi).
gatherWumpi([]).

gatherDyingWumpi(DyingWumpi) :-
	setof(wumpus_dying(Wx,Wy),wumpus_dying(Wx,Wy),DyingWumpi).
gatherDyingWumpi([]).

gatherGolds(Gold) :-
	setof(gold(Gx,Gy),gold(Gx,Gy),Gold).
gatherGolds([]).

getPlayer([player(Px,Py,PFDir)]) :-
	player(Px,Py,PFDir).
getPlayer([]).

getPlayerShot([player_shot(X,Y,Direction)]) :-
	player_shot(X,Y,Direction).
getPlayerShot([]).

/* -------------------------------------------------------------------------- */
/* Perception */

percept(Percepts) :-

	% Bind player coordinates
	player(X,Y,_),
	perceptBump(Bump),
	perceptScream(Scream),
	perceptBreeze(X,Y,Breeze),
	perceptStench(X,Y,Stench),
	perceptGlitter(X,Y,Glitter),
	append([Bump,Breeze,Stench,Glitter,Scream], Percepts).

percept([]). % Used when player exited.

perceptBump([bump]) :- bump.
perceptBump([]).

perceptScream([scream]) :- scream.
perceptScream([]).

perceptBreeze(X,Y,[breeze]) :- breeze(X,Y).
perceptBreeze(_,_,[]).

perceptStench(X,Y,[stench]) :- stench(X,Y).
perceptStench(_,_,[]).

perceptGlitter(X,Y,[glitter]) :- gold(X,Y).
perceptGlitter(_,_,[]).


/* -------------------------------------------------------------------------- */
/* Utility functions */
/* -------------------------------------------------------------------------- */

adjacent(WrldX, WrldY, Bx,By,X,Y) :-
	X is Bx-1, Y is By  , X >= 1;
	X is Bx+1, Y is By  , X =< WrldX;
	X is Bx  , Y is By-1, Y >= 1;
	X is Bx  , Y is By+1, Y =< WrldY.

range(Min,Max,Range) :- accRange(Min,Max,Max,[],Range).
accRange(Min,_,Min,Acc,[Min|Acc]).
accRange(Min,Max,Curr,Acc,Range) :-
	Curr > Min,
	Next is Curr-1,
	accRange(Min,Max,Next,[Curr|Acc], Range).

listProduct(List1,List2,OutList) :-
	accListProduct(List1,List2,[],OutList).
accListProduct([],_,Acc,RAcc) :-
	reverse(Acc,RAcc).
accListProduct([H|T],List2,Acc,OutList):-
	appendToList(H,List2,HList2),
	reverse(HList2,RHList2),
	append(RHList2,Acc,NewAcc),
	accListProduct(T,List2,NewAcc,OutList).

appendToList(Item,List,OutList) :-
	accAppendToList(Item,List,[],OutList).
accAppendToList(_,[],Acc,RAcc) :-
	reverse(Acc,RAcc).
accAppendToList(Item,[H|T],Acc,OutList):-
	accAppendToList(Item,T,[[Item,H]|Acc],OutList).

delete([], _, []) :- !.
delete([Elem|Tail], Elem, Result) :- !,
	delete(Tail, Elem, Result).
delete([Head|Tail], Elem, [Head|Rest]) :-
	delete(Tail, Elem, Rest).

/* -------------------------------------------------------------------------- */
/* Utility functions - Copied from SWI-Prolog Library */
/* -------------------------------------------------------------------------- */

%	append(+ListOfLists, ?List)
%
%	Concatenate a list of lists.  Is  true   if  Lists  is a list of
%	lists, and List is the concatenation of these lists.
%
%	@param	ListOfLists must be a list of -possibly- partial lists

append([], []).
append([L|Ls], As) :-
	append(L, Ws, As),
	append(Ls, Ws).
