Project Title: Chess AI with an interface for user play

Group Members: Akshay Sharma (acs1246), Akshay Kalbhor (abk7766)

Structure of the Code:

i) Files with minimal code but pivotal types:
   
   a) Colour    - Contains the type and a basic function to do with colouring the chess pieces.
   b) PieceType - Contains the type and a basic function to do with the various different kinds of pieces in chess.
   c) Piece     - Contains the compound type that combines colours and piecetypes to produce the actual game pieces and related functions.
   d) Parser    - Presently a primitive parser which gets the job done in a simple manner for all manner of piece movements, currently does not allow castling.
                  Returns a value which is a tuple whose first element is the type of piece being moved and the second element is itself a tuple that has the x and y coordinates of the destination.

ii) Files which have currently have more code than they probably should. Probably need to be refactored soon.

    a) Board    - Intended to contain types and functions to do with the chess board and movements within the chess board. Now contains all that and more functions to do with validations,
                  chess rules, movement generation, board display, etc. It contains pivotal type definitions such as those for the board, a square on the board, positions on the board,
                  game state (which represents the game at the current point), game history and the overall game (combination of game state, game history and the colour of the AI player).
    b) Game     - Intended to contain the 'main' function which would act as the point of entry to the system and begin the game. Currently also contains functions to run the game and
                  to create/produce movements for each of the players.

iii) Tests      - Contains a small number of HUnit tests.

Dependency Structure:

The Colour, PieceType and Parser modules are standalone and should remain so.
The Piece module depends on Colour and PieceType.
The Board module depends on all of the above mentioned modules as well as the System.Random module which is uses to currently produce random moves for the AI player.
The Game module depends on all of the above mentioned modules as well as the Text.Read module to help with IO.

Current functionality:

Ability to display the board, gives the player the choice between black or white pieces, takes in input from the player in standard chess notation (Ex: "Nf3", "a3", "Qc2", etc)
and moves the pieces as required after parsing (does not currently support castling as a move from the player), generates a random move for the AI, currently has rules implemented for the movements of all pieces (plus enpassant capture, castling, checks for the 'check' condition). Currently, it does not check for the checkmate condition.

In order to run the project: Navigate to the folder with the project files and load/run the Game file. (On ghci, run the main function). Currently, all inputs to the program need to be entered in quotes for it to be able to parse them. Also, error handling has not been implemented as much as we would have liked. So, entering valid moves helps the game go on.

General Questions:

1) Would it be sufficient to have a random generation AI and a min-max/alpha-beta pruning AI along with the testing infrastructure and the parser?
2) Would you have any other suggestions about how we go about accomplishing the rest of the requirements? Particular areas of more focus plus aspects that would make the project a suitably challenging one on technical terms.
3) We intend to separate out the current random movement generator into its own module where it will be our first 'AI'. Currently, we have not thought too much about how we intend to handle multiple AIs and would be glad to get ideas about modelling this portion of the system.

Feedback Questions:

1) Suggestions to better modularize our code.
2) Specific suggestions for additions to the code.
3) Specific suggestions for improvements to the code.
4) Possible places where we can make more/better use of higher order functions and the other staples of functional programming.