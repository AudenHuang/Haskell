module Main where

import World
import Actions
import Parsing

import Control.Monad
import Control.Monad.Trans
import System.IO
import System.Console.Haskeline
import Data.List
import System.Exit

winmessage = "Congratulations, you have made it out of the house.\n" ++
             "Now go to your lectures..."

{- Given a game state, and user input (as a list of words) return a 
   new game state and a message for the user. -}

process :: GameData -> String -> (GameData, String)
process state input = case runParser input of
                            Just cmd -> actions cmd state
                            Nothing -> (state, "I don't understand")

repl :: GameData -> InputT IO GameData
repl state | finished state = return state
repl state = do lift $ print state
                lift $ putStr "What now? "
                lift $ hFlush stdout
                cmd <- getInputLine "> "
                case cmd of
                   Nothing -> return state
                   Just "save" -> do lift $ save state
                                     outputStrLn "Saved"
                                     repl state
                   Just cmd -> do let (state', msg) = process state cmd
                                  outputStrLn msg
                                  if (won state') then do outputStrLn winmessage
                                                          return state'
                                                else repl state'
                {-if cmd == "save" then do save state
                                         putStrLn "Saved"
                                         repl state
                else do
                let (state', msg) = process state cmd
                outputStrLn msg
                if (won state') then do outputStrLn winmessage
                                        return state'
                               else repl state' -}

--save :: GameData -> IO GameData
save gd = do writeFile "save_data.txt" (show (location_id gd) ++ "\n" ++ show (inventory gd) ++ "\n" ++ show (poured gd) ++ "\n" ++ show (caffeinated gd))
             return gd

-- Valid words for tab completion
validWords :: [String]
validWords = ["go","get", "put", "examine", "pour", "drink", "open", "wear", "use", "press", "inventory", "quit"]

-- Given the string so far, return a list of possible combinations from
-- 'validWords'
completeValid :: String -> [Completion]
completeValid inp = map simpleCompletion (filter (isPrefixOf inp) validWords)

--Use the entire input so far, passing it in to 'completeValid'
myCompletions :: CompletionFunc IO
myCompletions (left, right) = return ([], --the input we didn't use
                                   completeValid (reverse left))

tes :: InputT IO ()
tes = do repl initState
         return ()


main :: IO ()
main = runInputT (setComplete myCompletions defaultSettings) tes
