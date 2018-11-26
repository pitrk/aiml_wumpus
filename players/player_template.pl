/* Jovolog: Template for creating agent programs for the wumpus world based on
  Artifical Intelligence, A Modern Approach, Second Edition
  Chapter 7.2: The Wumpus World
  */

/* Iterates player, generating player action and knowledge for next call of 
act/2. 

At the moment of calling act/2 knowledge base contains predicates from
PlayerKnowledge list and PlayerPercepts list from worlds' generate_world/3 
predicate, if act/2 was called for the first time.

If it is second or later call, knowledge base contains predicates from 
Knowledge list from previous act/2 call and predicates from PlayerPercepts list
from world's react/3 call.
  
  ----------------------------------------------------------------------------
  Available actions:
  ----------------------------------------------------------------------------
  
  moveForward	- stays in place if there is a wall in front of the agent,
  turnLeft	- turn left 90 degrees,
  turnRight	- turn right 90 degrees,
  shoot		- agent shoots arrow in direction she is facing,
    Shot arrow flies by cells in straight line until it hits a wall or wumpus.
    If wumpus is hit, he dies, and cell containing it becomes safe to enter.
    Agent has only one arrow, so only the first shoot action has any effect.
  grab		- agent can grab a gold if stands in a cell containing it,
  exit		- agent can exit cave if she is in a starting cell (1,1).
  
  ----------------------------------------------------------------------------
  Percepts:
  ----------------------------------------------------------------------------
  
  The following percepts may be asserted in the agent's knowledge base
  when act/2 is called.
  
  stench	- if agent is adjacent to Wumpus or stands on a dead wumpus,
  breeze	- if agent is adjacent to pit,
  glitter	- if agent is in a cell with gold,
  bump		- if agent just tried to walk into a wall,
  scream	- if agent just killed the wumpus by shooting an arrow.
  
  worldSize(X,Y) - special percept available in agent knowledge base only
    at the first call to act/2.  X and Y are integers denoting world size.
  
  Note:
  cells are "adjacent" if they differ in exactly one coordinate by 1, in
  particular, cells placed diagonally are NOT adjacent.
  
*/

act(Action, Knowledge) :-

	Action = none,
	Knowledge = [].
