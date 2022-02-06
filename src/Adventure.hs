module Main where

import World
import Actions ( actions )
import Parsing ( runParser )

import Control.Monad
import System.IO ( hFlush, stdout )
import System.Exit

winmessage = "Congratulations, you have made it out of the house.\n" ++
             "Now go to your lectures..."

{- Given a game state, and user input (as a list of words) return a 
   new game state and a message for the user. -}

process :: GameData -> String -> (GameData, String)
process state input = case runParser input of
                            Just cmd -> actions cmd state
                            Nothing -> (state, "I don't understand")

repl :: GameData -> IO GameData
repl state | finished state = return state
repl state = do startgame state
                putStr "\nWhat now?\n\n"
                hFlush stdout
                cmd <- getLine
                if cmd == "save" then do save state
                                         putStrLn "Saved"
                                         repl state
                else do
                let (state', msg) = process state cmd
                putStrLn msg
                if won state' then do putStrLn winmessage
                                      return state'
                               else repl state'



startgame :: GameData -> IO ()
startgame state = if lighton state 
                  then do print state
                  else do putStrLn "The room is dark you can't see anything.\nYou can feel a light switch next to your hand.\nPleas press the switch to turn the lights on."


save :: GameData -> IO GameData
save gd = do writeFile "save_data.txt" (show (location_id gd) ++ "\n" ++ show (inventory gd) ++ "\n" ++ show (poured gd) ++ "\n" ++ show (caffeinated gd))
             return gd




main :: IO ()
main = do repl initState
          return ()
