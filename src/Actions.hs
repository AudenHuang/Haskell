module Actions where

import World
import Data.Maybe
{-import Data.List-}

actions :: Command -> Action
actions (Go dir)      = go dir
actions (Get obj)     = get obj
actions (Put obj)     = put obj
actions (Pour obj)    = pour obj
actions (Examine obj) = examine obj
actions (Drink obj)   = drink obj
actions (Open obj)    = open obj
actions Quit          = quit
actions Inventory     = inv


{- Given a direction and a room to move from, return the room id in
   that direction, if it exists.

e.g. try these at the ghci prompt

*Main> move "north" bedroom
Just "kitchen"

*Main> move "north" kitchen
Nothing
-}

move :: Direction -> Room -> Maybe RoomID
move dir rm | length [ x | x <- exits rm, exit_dir x == dir ] == 1    = let theRoom = [ x | x <- exits rm, exit_dir x == dir ]
                                                                        in  Just (room (head theRoom))     
            | otherwise                                               = Nothing

{- Return True if the object appears in the room. -}

objectHere :: ObjectID -> Room -> Bool
objectHere o rm = foldr (||) False  [ o == obj_name object | object <- objects rm ]

{- Given an object id and a room description, return a new room description
   without that object -}

removeObject :: ObjectID -> Room -> Room
removeObject o rm = Room (room_desc rm) (exits rm)  (filter (\x -> obj_name x /= o) (objects rm))

{- Given an object and a room description, return a new room description
   with that object added -}

addObject :: Object -> Room -> Room
addObject o rm = Room (room_desc rm) (exits rm) ((objects rm) ++ [o])

{- Given an object id and a list of objects, return the object data. Note
   that you can assume the object is in the list (i.e. that you have
   checked with 'objectHere') -}

findObj :: ObjectID -> [Object] -> Object
findObj o ds = head (filter (\x -> (obj_name x) == o) ds)

{- Use 'findObj' to find an object in a room description -}

objectData :: ObjectID -> Room -> Object
objectData o rm = findObj o (objects rm)

{- Given a game state and a room id, replace the old room information with
   new data. If the room id does not already exist, add it. -}

updateRoom :: GameData -> RoomID -> Room -> GameData
updateRoom gd rmid rmdata = let newWorld = filter (\x -> fst x /= rmid) (world gd)
                                newRoom = Room (room_desc rmdata) (exits rmdata) (objects rmdata)
                                in GameData (location_id gd) (newWorld ++ [(rmid, newRoom)]) (inventory gd) (poured gd) (caffeinated gd) (finished gd)


{- Given a game state and an object id, find the object in the current
   room and add it to the player's inventory -}

addInv :: GameData -> ObjectID -> GameData
addInv gd obj = GameData (location_id gd) (world gd) (inventory gd ++ [objectData obj (getRoomData gd)]) (poured gd) (caffeinated gd) (finished gd) 

{- Given a game state and an object id, remove the object from the
   inventory. Hint: use filter to check if something should still be in
   the inventory. -}

removeInv :: GameData -> ObjectID -> GameData
removeInv gd obj = GameData (location_id gd) (world gd) (filter (\x -> obj_name x /= obj) (inventory gd)) (poured gd) (caffeinated gd) (finished gd) 

{- Does the inventory in the game state contain the given object? -}

carrying :: GameData -> ObjectID -> Bool
carrying gd obj = foldr (||) False [ obj == obj_name o | o <- inventory gd ]

{-
Define the "go" action. Given a direction and a game state, update the game
state with the new location. If there is no exit that way, report an error.
Remember Actions return a 2-tuple of GameData and String. The String is
a message reported to the player.

e.g.
*Main> go "north" initState
(kitchen,"OK")

-}

go :: Direction -> Action
go direction gd | isJust (move direction (getRoomData gd))  = (gd {location_id = fromJust (move direction (getRoomData gd))}, "OK")
                | otherwise                                 = (gd, "You can't go this way. It's a dead end.")

{- Remove an item from the current room, and put it in the player's inventory.
   This should only work if the object is in the current room. Use 'objectHere'
   and 'removeObject' to remove the object, and 'updateRoom' to replace the
   room in the game state with the new room which doesn't contain the object.

   Hints: you will need to take care to update things in the right order here!
    * create a new state with the updated inventory (use 'objectData')
    * create a new room without the object (use 'removeObject')
    * update the game state with this new room in the current location
      (use 'location_id' to find where the player is)
-}

get :: ObjectID -> Action
get obj state = let currentRoomName = location_id state
                    currentRoom = getRoomData state
                    exist = objectHere obj currentRoom
                    newInv =   addInv state obj
                    newRoom = removeObject obj currentRoom
                    newGameState = updateRoom newInv currentRoomName newRoom
                    in if exist then (newGameState, "You've picked up the item")
                       else (state, "There's no such item.")

{- Remove an item from the player's inventory, and put it in the current room.
   Similar to 'get' but in reverse - find the object in the inventory, create
   a new room with the object in, update the game world with the new room.
-}

put :: ObjectID -> Action
put obj state = let newInvGS = removeInv state obj
                    currentRoomName = location_id  newInvGS
                    currentRoom = getRoomData state
                    object = filter (\x -> obj_name x == obj) (inventory state) !! 0
                    newRoom = addObject object currentRoom
                    newGameState = updateRoom newInvGS currentRoomName newRoom
                    in (GameData (location_id newInvGS) (world newGameState) (inventory newInvGS) (poured newInvGS) (caffeinated newInvGS) (finished newInvGS), "object dropped")

{- Don't update the state, just return a message giving the full description
   of the object. As long as it's either in the room or the player's 
   inventory! -}

examine :: ObjectID -> Action
examine obj state = let currentRoom = getRoomData state
                        isObjInInv = carrying state obj
                        isObjInRoom = objectHere obj currentRoom
                        object | isObjInInv  = findObj obj (inventory state)
                               | isObjInRoom = findObj obj (objects currentRoom)
                        in if isObjInRoom || isObjInInv then (state, obj_desc object) 
                           else (state, "There's no such item.")

{- Pour the coffee. Obviously, this should only work if the player is carrying
   both the pot and the mug. This should update the status of the "mug"
   object in the player's inventory to be a new object, a "full mug".
-}

pour :: ObjectID -> Action
pour obj state | mug `elem` inventory state && coffeepot `elem` inventory state && obj == Coffee        = (state{inventory = filter (/= mug) (inventory state) ++ [fullmug], poured = True}, "Coffee poured!")
               | fullmug `elem` inventory state && obj == Coffee                                        = (state, "The mug is already full")
               | mug `elem` inventory state && coffeepot `notElem` inventory state && obj == Coffee     = (state, "You need a coffee pot" ) 
               | mug `notElem` inventory state && coffeepot `elem` inventory state && obj == Coffee     = (state, "You need a mug")
               | otherwise                                                                              = (state, "What are you trying to pour?")

{- Drink the coffee. This should only work if the player has a full coffee 
   mug! Doing this is required to be allowed to open the door. Once it is
   done, also update the 'caffeinated' flag in the game state.

   Also, put the empty coffee mug back in the inventory!
-}

drink :: ObjectID -> Action
drink obj state | fullmug `elem` inventory state && obj == Coffee   = (state {inventory = filter (/= fullmug) (inventory state) ++ [mug], caffeinated = True}, "Coffee drank")
                | obj /= Coffee                                     = (state, "What are you traing to drink")
                | otherwise                                         = (state, "There's no coffee")

{- Open the door. Only allowed if the player has had coffee! 
   This should change the description of the hall to say that the door is open,
   and add an exit out to the street.

   Use 'updateRoom' once you have made a new description. You can use 
   'openedhall' and 'openedexits' from World.hs for this.
-}

open :: ObjectID -> Action
open Door state   | caffeinated state && key `elem` inventory state && location_id state == Hall     = (updateRoom state Hall hall {room_desc = openedhall, exits = openedexits}, "You've opened the door with the key")
                  | caffeinated state && key `notElem` inventory state && location_id state == Hall  = (state,  "You don't have the key")
open _    state                                                                                      = (state, "It doesn't make sense to do so!")

{- Don't update the game state, just list what the player is carrying -}

inv :: Action
inv state = (state, showInv (inventory state))
   where showInv [] = "You aren't carrying anything"
         showInv xs = "You are carrying:\n" ++ showInv' xs
         showInv' [x] = obj_longname x
         showInv' (x:xs) = obj_longname x ++ "\n" ++ showInv' xs

quit :: Action
quit state = (state { finished = True }, "Bye bye")