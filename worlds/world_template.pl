/* Jovolog: Template for creating world */

/*

When running simulation, predicate calls and operations are performed in
following order:

(KB = knowledge base)

* call default_world_params(Params)
* append Params list contents to world KB
* call generate_world(GeneratedWorld, PlayerKnowledge, PlayerPercepts)
* append GeneratedWorld list contents to world KB
* append PlayerKnowledge and PlayerPercepts list contents to player KB
* call points(Points)
* call generate_display(DisplaySize, Display)
* Label: LOOP (break point between simulation steps)
	* call act(Action, Knowledge) (on agent)
	* append player_action(Action) to world KB
	* append Knowledge list list contents to player KB
	* call react(GameStatus, NextWorld, PlayerPercepts)
	* append NextWorld list contents to world KB
	* append PlayerPercepts list contents to player KB
	* call points(Points)
	* call generate_display(DisplaySize,Display)
* If last GameStatus was continue(Msg), jump to LOOP. Otherwise end simulation.

Note: all predicates which are not needed or outdated are removed accordingly.

*/

/* -------------------------------------------------------------------------- */
/* Default world parameters */
/* -------------------------------------------------------------------------- */
/* Generates Params, a list of predicates which will be visible in application 
GUI and manually editable by user. 

These predicates (optionally changed by user in GUI) will be available in world
knowledge base when predicate generate_world/2 will be called. 
*/

default_world_params(Params) :- 
	Params = [].

/* -------------------------------------------------------------------------- */
/* Initial world generation */	
/* -------------------------------------------------------------------------- */
/* Generates initial world, inital player knowledge and initial player percepts.

Predicates from GeneratedWorld list will be present in world knowledge base 
when react/3 will be called for the first time.

Predicates from PlayerKnowledge and PlayerPercepts lists will be present
in player knowledge base when act/2 will be called for the first time.
*/


generate_world(GeneratedWorld, PlayerKnowledge, PlayerPercepts) :-
	GeneratedWorld = [],
	PlayerKnowledge = [],
	PlayerPercepts = [].
	
/* -------------------------------------------------------------------------- */
/* World display generation */
/* -------------------------------------------------------------------------- */
/* Generates predicates for application, defining how to visualize world.

DisplaySize must be a (X,Y) pair, where X,Y are natural numbers defining 
size of grid which will be displayed.

Display must be a list of triples of form (X,Y,'image_name.ext'), where
X,Y define coordinates of visualized grid cell and 'image_name.ext' is name
of image in "./images/" catalog. 

Example:
DisplaySize = (2,2)
Display = 
[(1,1,'player.bmp'),(1,2,'stench.bmp'),(2,1,'gold.bmp'),(2,2,'wumpus.bmp')]

WARNING: player_action(Action) is unavailable in world knowledge base
at time of calling generate_display/2.

*/

generate_display(DisplaySize,Display) :-
	DisplaySize = (1,1),
	Display = [(1,1,'empty.bmp')].
	
/* -------------------------------------------------------------------------- */	
/* Player points retrieval */
/* -------------------------------------------------------------------------- */
/* Unifies variable Points to current player points, visible in GUI. 

WARNING: player_action(Action) is unavailable in world knowledge base
at time of calling points/1.
*/

points(Points) :- 
	Points = 0 .
	
/* -------------------------------------------------------------------------- */	
/* World iteration */
/* -------------------------------------------------------------------------- */
/* Iterates world, generating game status for application, next world for
next call of react/3 and player percepts for next call of act/2 on player.

At the moment of calling react/3 knowledge base contains predicates from
GenerateWorld list (see generate_world/3), if react/3 was called for the
first time.

If it is second or later call, knowledge base contains predicates from
NextWorld list from previous react/3 call and player_knowledge(A) predicate,
where A is value which unfied to Action in act/2 call on player.

GameStatus must unify to continue(Msg) or end(Msg), where Msg is char sequence
enclosed in single quotes. 

continue(Msg) means simulation should continue.
end(Msg) means simulation should end.
In both cases Msg is shown in application's game log.

NextWorld must be a list of predicates. NextWorld list contents will be present
in world knowledge base when react/3 will be called again.

PlayerPercepts must be a list of predicates. PlayerPercepts list contents will
be available in player knowledge base when act/2 will be called.
*/

react(GameStatus, NextWorld, PlayerPercepts) :-
	GameStatus = end('example end reason'),
	NextWorld = [],
	PlayerPercepts = [].
