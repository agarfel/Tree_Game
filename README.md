# PDV101

## Random Mode

In this game, you become a student in the bachelor. You are given choices, which lead to different outcomes, affecting the number of credits and the happinness of your character. You can also choose to undo (back) a choice to take a different path.

Internally, this is represented as a binary tree, which is generated as you play, and stored so you can explore the different paths you could have taken. You can view the tree using "see".

The code is split into multiple parts:

1. Data Types Definitions
    1. For the tree:
        1. BinTree a b
           A binary tree with labeled nodes of type b and leaves of type a. The game's binary tree is of type BinTree Int Event
       2. BinCxt a b
          A one hole context to have BinTree Zippers
       3. BinZip a b
          A binary tree zipper that allows us to travel through the game tree.
    2. For the game:
       1. Stats
          Stores the player's information: credits, happiness and number of returns
       3. Effect
          The effect a choice has on your stats
       5. Event
          Stores the event and the two options, as well as the effect each option has.

2. Methods for Data Types
    1. Stats and Effects
       1. UpdateStats : apply effect to stats
       2. undoStats: undo effect on stats and increase number of returns
       3. checkOver : check if the player has won or lost
    2. BinTree Zipper
       1. go_left, go_right, go_down: used to navigate the Tree (zipper)
       2. treeFromBin, treeCxtFromBinCxt, treeFromBinZip, drawBin, drawBinZip: used to display the whole Tree and the current place.
      
    3. Event:
       1. getEvent : uses an IO monad to randomly select an Event from a list of Events
      
3. Initial Values
   1. Initialise list of possible events
   2. Initialise game tree (Empty tree)
   3. Initialise stats
  
4. Game Functions
   1. move : Takes a BinZip, a function (direction to move) and stats, moves through zipper and calls go
   2. get_input : reads input from user and calls respective functions
   3. get_mention : chooses (prints) ending
   4. go: game loop
   5. play : prints instructions and starts game loop
