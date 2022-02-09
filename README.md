# Haskell
This is the first practical for CS2006. This folder has a .cabal file for building
and testing the game. The src folder contain all the .hs files for the game and for testing.

## Requirements
QuickCheck must be installed to run the test for the game. We recommend using cabal to install it using the terminal
with the following command.

    $ cabal install QuickCheck
    
## Game
To run the game, cd to the Haskell folder and run the following commands
    
    $ cabal build
    $ cabal run adventure

## Test
To run the test, cd to the Haskell folder and run the following commands
    
    $ cabal build
    $ cabal run test
