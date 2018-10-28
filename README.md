This is a battleship implemented in Scala by Hamelina Julia EHAMELO

1. Instructions

  In the repository given above, you can find a main program: Main.scala. This latter handles the following scenarios according to what you want to do:
    - a game between 2 humains
    - a game between 1 human and 1 AI
    - 3 games between AIs

  1.1. Requirements

    Before compiling and running the program, you have to make sure you have sbt installed on your computer. If not yet installed, please follow the installation phase of this tutorial: https://docs.scala-lang.org/getting-started-sbt-track/getting-started-with-scala-and-sbt-on-the -command-line.html​.
  1.2. How does it work ?
    The source code is available in the following repository:
    https://github.com/Hamelina/battleship
    Then here are the steps you need to follow to make it run:
    
      ● First make sure you have the environnement set up (1.
      
      ● Clone or download the repository on you computer
      
      ● Open the terminal and type the following command:
        
        $ cd battleship $ sbt
        
        $ run Main
        
    Then, according to what what you would like to do, you have to following the instructions.

  1.3. The rules

    The rules of this game is the following:
      - you play on a grid of 10x10
      - depending on the game mode you choose, you have to place 6 ships.
      - each ship cannot be superposed to another one, but they can be placed near each
      one
      - you are not allowed to quit the game
      - you can play against either as a human, an AI and you can make AIs play again each
      other.
      
2. AIs

  2.1. AI Level Beginner
  
    The AI level beginner is an AI that does not have any memory at all. It shoots squares randomly (even if it has already been targeted before). This way it does not “think”. It only knows the limits of the grid to target.
    
  2.2. AI Level Medium
  
    On the contrary of the AI Level Beginner, the AI Level Medium has a memory of the cells on which it has already shoot. But it still choose randomly its targets. This way the AI Level Medium is “smarter” than the Level Beginner one.
  
  2.3. AI Level Hard
  
    The AI Level Hard is like the AI Level Medium with its memory of the already targeted cells but it keeps something else in memory.
    At the beginning of the game, it shoots randomly. Then, when this AI hits a square, it does the following operation:
      - calculates “potential cells” that might be occupied, whether this cells are in or out of the grid (up, down, to the right and to the left of the hit cell concerned)
      - filters the cells that are “valid” (inside the grid)
      - affect the valid cells to a list of cell which will be targeted next time it have to shoot.
      - If the cell have already been targeted, it checks the next one until there is nothing
      else.
      - only then it targets randomly.
