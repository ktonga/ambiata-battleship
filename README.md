Ambiata Take-Home Coding Exercise (Battleship) Implementation
=============================================================

### Design

The Object `Battleship` holds the API for controlling the game, the operations that change the state of the game
are expressed as State Monad Actions, and the rest of the operations can be achieved just navigating the last
returned State which is a `Game` case class.
It is done this way so that it can be easily integrated into a FP Game Loop. No trampolining is needed since
just a few hundred transitions are required to finish the game.

In the tests `BattleshipSpec` some examples of how to use the API can be found.

I gave up with the top-level validations and error-handling as it was taking longer than expected to complete
the exercise.


### Running the tests

In the project folder run the following command:

    ./sbt test


