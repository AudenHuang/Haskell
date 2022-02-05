module Main where

import World
import Actions
import Parsing

import Control.Monad
import System.IO
import System.Exit
import System.Directory (doesFileExist)
import Distribution.Simple.Utils (lowercase)
import Control.DeepSeq (rnf)


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
repl state = do print state
                putStr "What now? "
                hFlush stdout
                cmd <- getLine
                if cmd == "save" then do save state
                                         putStrLn "Saved"
                                         repl state
                else if cmd == "load" then do putStrLn "Loaded Save Data"
                                              load
                else do
                let (state', msg) = process state cmd
                putStrLn msg
                if (won state') then do putStrLn winmessage
                                        return state'
                               else repl state'

save :: GameData -> IO GameData
save gd = do writeFile "save_data.txt" (show (location_id gd) ++ "\n" ++ show (inventory gd) ++ "\n" ++ show (poured gd) ++ "\n" 
               ++ show (caffeinated gd) ++ "\n" ++ show (objects (getIndivRoom gd Bedroom)) ++ "\n" ++
                show (objects (getIndivRoom gd Kitchen)) ++ "\n" ++ show (objects (getIndivRoom gd Hall)) ++ "\n" ++ show (objects (getIndivRoom gd LivingRoom)))
             return gd



load = do inFile <- openFile "save_data.txt" ReadMode
          gd <- hGetContents inFile
          rnf gd `seq` hClose inFile
          let saveData = lines gd
              defState = GameData Bedroom gameworld [] False False False
              currentRoom = parseRoom (saveData!!0)
              currentInv = parseInv (splitInv (saveData!!1))
              pourState = parseBool (saveData!!2)
              caffState = parseBool (saveData!!3)
              wrld = [(Bedroom, worldState defState Bedroom (splitInv(saveData!!4))),
                      (Kitchen, worldState defState Kitchen (splitInv(saveData!!5))),
                      (Hall, worldState defState Hall (splitInv(saveData!!6))),
                      (LivingRoom, worldState defState LivingRoom (splitInv(saveData!!7))),
                      (Street, street)]
              initState = GameData currentRoom wrld currentInv pourState caffState False
          do repl initState

worldState :: GameData -> RoomID -> [[Char]] -> Room
worldState gd rmid items = Room (room_desc (getIndivRoom gd rmid))
                                (exits (getIndivRoom gd rmid))
                                (parseInv items)
                                         

main :: IO ()
main = do repl initState
          return ()
