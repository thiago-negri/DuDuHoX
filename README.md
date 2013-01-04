DuDuHoX
=======

Making a maze game in order to learn Haskell.

## Game ##

- Player is represented by '@'.
- Player can move around pressing W, A, S and D keys.
  - Can move only on top of walkable floor (represented by '.').
- Player can quit the game pressing Q key.
- To win the game, move on top of exit (represented by '!').

## Progress ##

### OK ###
- Movement: W, A, S, D.
- Quitting: Q.
- Winning: Moving onto '!'.
- World parsing: Matrix of char -> World.
- Line of sight: Bresenham's algorithm.
- Fog of war: Seen objects are grayed out when they go out of sight.
- Draw walkable floor: Walkable floor ('.') is not the same as empty space (' '); Empty space do not block vision but can't be moved onto.
- User interface (world display area, controls, message area): ASCII art.
- World scrolling: Player is always at the center of the screen.
- Console user interface: The first release, with all features.
- OpenGL user interface: Playable, still missing features of the Console version.

### Things to do ###
- Improve user interfaces code: Make a core game module that handles the state of the game. The user interfaces modules should only provide "receive input" and "render" functions.
- Improve OpenGL user interface: Add features from the Console version (world scrolling, ...).
- Graphical world editing: Create new worlds inside the game.
- Save game: Save your progress and resume later.
- Interactive objects: keys and doors.
- Circuits: Add levers connected to doors.
