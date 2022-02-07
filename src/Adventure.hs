module Main where

import World
import Actions ( actions )
import Parsing 

import Control.Monad
import Control.Monad.Trans
import Data.List
import System.Console.Haskeline
import System.IO
import System.Exit
import System.Directory (doesFileExist)
import Control.DeepSeq (rnf)

winmessage = "Congratulations, you have made it out of the house.\n" ++
             "Now go to your lectures..."
lossmessage = "Oh no! you died."

{- Given a game state, and user input (as a list of words) return a 
   new game state and a message for the user. -}

process :: GameData -> String -> (GameData, String)
process state input = case runParser input of
                            Just cmd -> actions cmd state
                            Nothing -> (state, "I don't understand")

repl :: GameData -> InputT IO GameData
repl state | finished state = return state

repl state = do lift $ startgame state
                lift $ putStr "\nWhat now?\n\n"
                lift $ hFlush stdout
                cmd <- getInputLine "> "
                case cmd of
                        Nothing -> return state
                        Just "save" -> do lift $ save state
                                          outputStrLn "Game Saved!"
                                          repl state
                        Just "load" -> do outputStrLn "Loading Save Data..."
                                          loadCheck state
                        Just cmd -> do let (state', msg) = process state cmd
                                       outputStrLn msg
                                       if won state' 
                                       then do outputStrLn winmessage
                                               return state'
                                       else if poisoned state'
                                       then do outputStrLn lossmessage
                                               return state'
                                       else repl state'



startgame :: GameData -> IO ()
startgame state = if lighton state 
                  then do print state
                  else do putStrLn "The room is dark you can't see anything.\nYou can feel a light switch next to your hand.\nPlease press the switch to turn the lights on."


save :: GameData -> IO GameData
save gd = do writeFile "save_data.txt" (show (location_id gd) ++ "\n" ++ show (inventory gd) ++ "\n" 
               ++ show (caffeinated gd) ++ "\n" ++ show (lighton gd)++ "\n" ++ show (maskon gd)++ "\n" ++
                show (objects (getIndivRoom gd Bedroom)) ++ "\n" ++ show (objects (getIndivRoom gd Kitchen)) ++ "\n" ++ 
                show (objects (getIndivRoom gd Hall)) ++ "\n" ++ show (objects (getIndivRoom gd LivingRoom))++ "\n" ++ 
                show (objects (getIndivRoom gd DinningRoom)))
             return gd
             

load :: InputT IO GameData
load = do inFile <- lift $ openFile "save_data.txt" ReadMode
          gd <- lift $ hGetContents inFile
          lift $ rnf gd `seq` hClose inFile
          let saveData = lines gd
              defState = GameData Bedroom gameworld [] False False False False False
              currentRoom = parseRoom (saveData!!0)
              currentInv = parseInv (splitInv (saveData!!1))
              caffState = parseBool (saveData!!2)
              lightState = parseBool (saveData!!3)
              maskState = parseBool (saveData!!4)
              wrld = [(Bedroom, worldState defState Bedroom (splitInv(saveData!!5))),
                      (Kitchen, worldState defState Kitchen (splitInv(saveData!!6))),
                      (Hall, worldState defState Hall (splitInv(saveData!!7))),
                      (LivingRoom, worldState defState LivingRoom (splitInv(saveData!!8))),
                      (DinningRoom, worldState defState DinningRoom (splitInv(saveData!!9))),
                      (Street, street)]
              initState = GameData currentRoom wrld currentInv caffState lightState maskState False False
          do repl initState


worldState :: GameData -> RoomID -> [[Char]] -> Room
worldState gd rmid items = Room (room_desc (getIndivRoom gd rmid))
                                (exits (getIndivRoom gd rmid))
                                (parseInv items)

loadCheck :: GameData -> InputT IO GameData
loadCheck state = do ifFile <- lift $ doesFileExist "save_data.txt"
                     if ifFile
                     then load
                     else do outputStrLn "No Save Data Found"
                             repl state


validWords :: [String]
validWords = ["go", "get", "put", "examine", "pour", "drink", "open", "wear", "use", "press", "inventory", "quit", "save", "load"]

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
