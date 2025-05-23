## Reactive Game

A basic game engine made with Functional Reactive Programming. Specifically, the Yampa library in Haskell.

DEPENDENCIES:
* stack
* SDL2 (+ ttf, image, gfx, mixer)
* pkg-config

RUN WITH:
* stack build
* stack exec reactivegame-exe

---

### Code layout

* **app/Main.hs** calls *reactimate* from **FRP.Yampa** using initialisation functions from **Utils.hs** (parsing configs and loading assets).
* **configs** folder contains the configuration files in yaml format.
* *GameConfigs* are immutable and accessible only from the *gameSF*. Constants that determine the logic of the game, such as win conditions, go in here. This allows them to be changed without the need for re-compilation.
* *DisplayConfigs* are immutable and accessible only from the *displayFunction*. Constants that determine the look and sound of the game, such as text colour, go in here. This allows them to be changed without the need for re-compilation.
* *DisplayResources* are immutable and accessible only from the *displayFunction*, but require some initialisation using the values in the *DisplayConfigs*. Textures and sound clips go in here.
* **Input.hs** provides the *detectInputs* function which polls the users inputs (since the last frame) and converts them into the *UserInputs* record type. This includes all mouse clicks and keyboard button presses stored as *InputKey*, *InputState* pairs (for if a key was was either just pressed, released, or is being held).
* The *EventPayload* type SDL provides is simplified to the *UserInputs* type using the *InputKey* type which is defined in **InputKey.hs**.
* We define *InputKey* to *InputAction* maps in our *GameConfigs* which are used to form *InputAction* to *InputState* maps. These latter maps are then useful in quickly looking up the state of each potential action in each mode of our *gameSF*. 
* **GameLogic.hs** provides the *gameSF* signal function that transforms *UserInputs* into *GameOutputs* whilst storing the state of the game. This SF is actually made up of a system of switches between identically typed SFs, one for each mode the game is in (playing, win-screen, lose-screen, etc.). Each mode can then have its own configs and *GameOutputs* subtype. Within these outputs is an *Event ModeSwitch* type value used to switch between game modes.
* Mutable values that need to be passed along between modes are stored in the *Baton* type which is contained in the *ModeSwitch* type. Each SF can access the *Baton* and (only) change it upon triggering a mode switch. Quantities like the RNG state, window dimensions, or previous level scores belong inside the baton.
* Finally, **Output.hs** takes the *GameOutputs* and renders them to the screen, using the *DisplayConfigs* and *DisplayResources*. This function is defined for each subtype of *GameOutputs* corresponding to each mode of the game.
* **Stochastic.hs** is a test-bed for some random process generating code I am experimenting with.  
* **LifeHash.hs** and **PatternParse.hs** are leftovers from the first project this engine was used for.

---

### To do now

1. Should `UserInputs` be processed so they're just a collection of `InputAction`? Then `switchModeDetect` could listen to them and catch them. But then, the game logic functions could just process lists..
1. Could we have a type representing each mode, which may make the routing code simpler? `ModeSwitch Mode Baton`
1. Switch to paused upon loss of focus
1. Resizeable window
1. Different modes may sometimes require different `DisplayResources`
1. `initial_offset` is in pixels, not grid-squares
1. 'Scroll-speed-multiplier' needs to be added back to the configs
1. `drawSquaresFromCoOrds` doesn't check if the squares are visible before sending their rendering instructions..
1. Anything in `Types.hs` that is only used by one module should probably only be in that module.
1. For clarity, fields are going to have simple names and qualified imports will avoid overlaps. This will probably mean breaking off types into their own module (also, not a bad thing).

---

### To do later

1. More sound effects from [freesoundorg](https://freesound.org/)
1. Setting the 'clip rectangle' of the renderer to the window size can be done, but doesn't seem to do anything? Maybe it is happening automatically under the hood.. or maybe the 'viewport' is doing that work?
1. [From 60FPS to 500](http://keera.co.uk/blog/2014/10/15/from-60-fps-to-500/)
1. Compare performance for different map types
