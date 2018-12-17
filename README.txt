Project Title: Chess AI with an interface for user play

Group Members: Akshay Sharma (acs1246), Akshay Kalbhor (abk7766)


Current functionality:

1) Ability to display the board
2) Gives the player the choice between black or white pieces
3) Gives the player the choice between player vs player, player vs AI, AI vs AI modes.
4) Gives the player the choice to save the game at any point.
5) Gives the player the choice initially to load a previously saved game.
6) Takes in input from the player in standard chess notation (Ex: "Nf3", "a3", "Qc2", "axb3", "Qxd4", "O-O", "O-O-O", "Kd3" etc).
7) Has the ability to read and replay (the cleaned up version) of an olf game.
8) Moves the pieces as required after parsing. 
9) Contains 2 AIs presently but need to be switched manually in order to play against each.
   a) The random AI just generates a random move each time.
   b) The minimax AI generates a game tree and chooses the best move from it based on the evaluators and their weights.
10) Currently has rules implemented for the movements of all pieces (plus enpassant capture, castling, checks for the 'check' ans 'check mate' conditions). 
11) The program should be able to handle most illegal inputs given to it with appropriate responses and recursing back to the original choice given to the user.

In order to run the project: Navigate to the folder with the project files and load/run the Game file. (On ghci, run the main function). 
When saving or loading files, they are automatically saved or loaded from the ChessGames directory which needs to be present in the given
directory strcuture currently.

Structure of the Code:

i) Files with minimal code but pivotal types:
   
   a) Colour    - Contains the type and a basic function to do with colouring the chess pieces.
   b) PieceType - Contains the type and a basic function to do with the various different kinds of pieces in chess.
   c) Piece     - Contains the compound type that combines colours and piecetypes to produce the actual game pieces and related functions.
   d) Parser    - Contains the functions used to parse moves entered by users during play as well as to parse game files (these could be save game files or 
                  files containing age old games that the user would like to play through).
   e) Move      - Contains the type which helps to separate out 'normal' moves from castling moves as well as a command that can be used by the player in
                  order to save the game during play.
   f) Ai        - Contains the types to do with the two AIs on offer.
   g) Minmax    - Contains the type and functions to do with game tree generation and running the minimax algorithm on it using the evaluators defined.
                  Currently, the heuristic is based on a weighted combination of piece analysis, central board analysis, as well as rook and bishop duos.

ii) Files which currently have more code than they probably should. And would benefit from refactoring.

    a) Board    - Contains types and functions to do with the chess board and movements within the chess board. Also has functions to do with validations,
                  chess rules, movement generation, board display, etc. It contains pivotal type definitions such as those for the board, a square on the board, positions on the board,
                  game state (which represents the game at the current point), game history, movement history (in Chess notation) and the overall game (combination of game state, game history and the colour of the AI player).
    b) Game     - Contains the 'main' function which acts as the point of entry to the system and begins with the menu followed by the game.
                  Currently also contains functions to run the game and to create/produce movements for each of the players.
                  It has an interactive menu system which allows the players to choose using various options.

iii) Tests      - Contains a number of HUnit tests.

Dependency Structure:

The Colour, PieceType and Parser modules are standalone and should remain so.
The Piece module depends on Colour and PieceType.
The Board module depends on all of the above mentioned modules as well as the System.Random module which is uses to currently produce random moves for the AI player as well as Data.List.
The Game module depends on all of the above mentioned modules as well as the Text.Read module to help with IO, System.Directory, Move and MinMax.
The Minmax module depends on Board, Piece, Colour, PieceType and Data.List.
The Move module depends on PieceType, Board and Colour.

Learning:

Over the course of the project, we learned a lot about 

1) The domain of Chess - Common methods of evaluating the board, ways to optimize board representation, different ways to encode chess movements, edge cases 
                         posed by the game and its myriad varieties of moves.
2) AI - Common methods of creating specialized AIs for board games including Minimax, Alpha-Beta pruning, Quiscent search and monte carlo tree search.
3) Haskell - The non-trivial amount of code that was required to be written gave us a better sense of how to structure Haskell programs (even if our
             structure eventually ended up being pretty bad and we ran out of time to clean it up adequately), better ideas about separation of the IO
             portion helps to simplify the overall project by isolating the impureness, Record types which represent structs and classes from other languages,
             dealing a lot more with immutable state and game representations which gave us a lot of flexibility to make 'limbo' moves that could be discarded
             if deemed unnecessary.

The roads not taken (Approaches attempted and not pursued):

1) I had a version of alpha-beta pruning working with a version of the minimax. It needed a lot more time to test and iron out the kinks. Would have added
   if more time was present.
2) I had more evaluators in a semi-developed state to add on to the heuristic in the minimax algorithm such as check mate move preference and check mate
   avoidance. They were also not done because of time constraints. These would be valuable additions to the present AI and server to improve it by a lot.
3) We had an old version of the parser that was kind of cobbled together for the mid-point and abandoned after because we went with a more modular approach.

Apart from these, other suggestions for people doing similar projects:

1) Look into Bitboards for efficient board representation.
2) Look into Maps instead of lists of lists for the boards.
3) 