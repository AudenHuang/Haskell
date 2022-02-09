{-# LANGUAGE TemplateHaskell #-}
module Main where
import Test.QuickCheck
import Test.QuickCheck.All
import Data.Maybe
import World
import Actions


instance Arbitrary Object where
    arbitrary = oneof [return mug,
                       return fullmug,
                       return coffeepot,
                       return mask,
                       return key,
                       return suspiciouscoffee]

instance Arbitrary ObjectType  where
    arbitrary = oneof [return Mug,
                       return Cup,
                       return Coffee,
                       return Mask,
                       return Key]

instance Arbitrary Direction where
    arbitrary = oneof [return North,
                       return South,
                       return East,
                       return West,
                       return Out]

instance Arbitrary Room where
    arbitrary = oneof [return bedroom,
                       return kitchen,
                       return hall,
                       return street,
                       return livingroom,
                       return dinningroom]

instance Arbitrary RoomID where
    arbitrary = oneof [return Bedroom,
                       return Kitchen,
                       return LivingRoom,
                       return DinningRoom,
                       return Hall,
                       return Street]

instance Arbitrary Command where
    arbitrary = frequency [(4, do dir <- arbitrary
                                  return (Go dir)),
                           (7, do obj <- arbitrary
                                  return (Get obj)),
                           (7, do obj <- arbitrary
                                  return (Put obj)),
                           (7, do obj <- arbitrary
                                  return (Examine obj)),
                           (7, do obj <- arbitrary
                                  return (Pour obj)),
                           (3, do obj <- arbitrary
                                  return (Drink obj)),
                           (3, do obj <- arbitrary
                                  return (Open obj)),
                           (3, do obj <- arbitrary
                                  return (Wear obj)),
                           (3, do obj <- arbitrary
                                  return (Press obj)),
                           (1, return Inventory),
                           (1, return Quit)]

instance Arbitrary GameData where
    arbitrary = return initState

{- Checking if RoomID type is returned when calling the move funciton. It also checks if "Nothing" is returned -}
prop_move :: GameData -> Direction -> Room -> Bool
prop_move gs dir rm | isJust(move dir rm) = fromJust (move dir rm) `elem` map fst (world gs)
                    | otherwise = isNothing (move dir rm)

{- Checking if the ObjectID is actaully an object from the room -}
prop_objectHere :: ObjectType  -> Room -> Bool
prop_objectHere objtype rm = objectHere objtype rm == (objtype `elem` map obj_name (objects rm))

{- Checking if is updated, with the object removed after calling the method -}
prop_removeObject :: ObjectType -> Room -> Bool
prop_removeObject objtype rm =  objectHere objtype (removeObject objtype rm) == False

{- Checking if object is added to the room when calling addObject function. -}
prop_addObject :: Object -> Room -> Bool
prop_addObject obj rm = objectHere (obj_name obj) (addObject obj rm)

{- Checking if the object is from the object list -}
prop_findObj :: Object -> [Object] -> Bool
prop_findObj obj ds = obj_name (findObj (obj_name obj) ( ds++ [obj])) == obj_name obj

{- Check if the room is updated using the world data -}
prop_updateRoom :: GameData -> RoomID -> Room -> Bool
prop_updateRoom gs rmid rm = let new_gs = updateRoom gs rmid rm
                             in getIndivRoom new_gs rmid == rm

{- Check if the carrying object is inside the inventory -}
prop_carrying :: GameData -> ObjectType  -> Bool
prop_carrying gs objtype = carrying gs objtype == (findObj objtype (inventory gs) `elem` inventory gs)

{- Check if the player is carrying the object that is just added to inventory -}
prop_addInv :: GameData -> ObjectType -> RoomID -> Bool
prop_addInv gs objtype rmid | objectHere objtype (getRoomData gs {location_id = rmid})       = carrying (addInv gs {location_id = rmid} objtype) objtype
                            | otherwise                                                      = True

{- Check if the player is NOT carrying the object that is just removed from the inventory -}
prop_removeInv :: GameData -> ObjectType -> Bool
prop_removeInv gs objtype = carrying (removeInv gs objtype) objtype == False

{- Check three conditions. 1. Is the light turned on? If not, the game will ask player to turn it on.
                           2. If the move returns a Just, return ok.
                           3. Otherwise, tell the player that it is a dead end.
-}
prop_go :: Direction -> GameData -> Bool
prop_go dir gs | not(lighton gs)                                                     = snd (go dir gs)  == "\nThe light is off. You can't move until you turn the light on\n"
               | isJust (move dir (getRoomData gs))                                  = snd (go dir gs)  == "\nOK\n"
               | otherwise                                                           = snd (go dir gs)  == "You can't go this way. It's a dead end."

{- Check three conditions  1. Is the light turned on? If not, the game will ask player to turn it on.
                           2. If the light is on, check if the object is in the room. If yes, the player will pick it up.
                           3. Otherwise, the game will tell the user that there is no such item.
-}
prop_get :: ObjectType -> GameData -> Bool
prop_get objtype gs | not(lighton gs)                                                = snd (get objtype gs) == "\nThe light is off. You can't see what's in the room until you turn the lights on\n"
                    | objectHere objtype (getRoomData gs)                            = snd (get objtype gs) == "\nYou've picked up the item\n"
                    | otherwise                                                      = snd (get objtype gs) == "There's no such item."

{- Check if the player is carrying the object (i.e. present in the inventory). If yes, the object will be dropped.
   Otherwise, the game will tell the user that the item is NOT in the player's inventory. 
-}
prop_put :: ObjectType -> GameData -> Bool
prop_put objtype gs | not (carrying gs objtype)                                      = snd (put objtype gs) == "This item isn't in your inventory. Are you tripping?"
                    | otherwise                                                      = snd (put objtype gs) == "object dropped"

{- Check if the object is in the inventory or in the room. If it is, then it should return the object description,
  else it should return "There's no such item."
-}
prop_examine :: ObjectType  -> GameData -> Bool
prop_examine objtype gs | carryObj || inRMObj = snd (examine objtype gs) == obj_desc obj
                        | otherwise           = snd (examine objtype gs) == "There's no such item."
       where rmid = getRoomData gs
             carryObj = carrying gs objtype
             inRMObj  = objectHere objtype rmid
             obj | carryObj = findObj objtype (inventory gs)
                 | inRMObj = findObj objtype (objects rmid)

{- Check if the ObjectType is not Coffee, the game should ask the player what he/ she would want to pour. If the player is not carrying a coffee pot, the game will
   remind him/ her. If there is a full mug, the game will tell the player that it is already full. If there is no mug, the game will tell the player that he/ she
   would need a mug. Otherwise, the coffee will be poured to the mug.
-}
prop_pour :: ObjectType -> GameData -> Bool
prop_pour objtype gs | objtype /= Coffee                                            = snd (pour objtype gs) == "What are you trying to pour???? Are you sure you are not high?"
                     | not (carrying gs Coffee)                                     = snd (pour objtype gs) == "You are not carrying a coffee pot. Are you tripping?"
                     | fullmug `elem` inventory gs                                  = snd (pour objtype gs) == "The mug is already full"
                     | mug `notElem` inventory gs                                   = snd (pour objtype gs) == "You need a mug"
                     | otherwise                                                    = snd (pour objtype gs) == "Coffee poured!"

{- Check if the ObjectType is not Coffee, the game should ask what the player would like to drink.
   If the suspicious cofee is in the inventory, the player will drink it and die. If the player doesn't
   have a mug, the game will remind him/ her to get it. If there is an empty mug, the game will suggest the
   player to pour  some coffee to the mug. If theplayer has drunk coffee already but have a full mug of coffee,
   we prevent the player from doing so. Otherwise, the player will drink the coffee. 
-}
prop_drink :: ObjectType -> GameData -> Bool
prop_drink objtype gs | objtype /= Coffee                                           = snd (drink objtype gs) == "What are you trying to drink"
                      | suspiciouscoffee `elem` inventory gs                        = snd (drink objtype gs) == "You've drank a cup of poison coffee."
                      | not (carrying gs Mug)                                       = snd (drink objtype gs) == "You don't even have a mug"
                      | mug `elem` inventory gs                                     = snd (drink objtype gs) == "Your mug is empty you mug. Pour some coffee into your mug first"
                      | fullmug `elem` inventory gs && caffeinated gs               = snd (drink objtype gs) == "You've drank a cup of coffee already. You shouldn't drink more"
                      | otherwise                                                   = snd (drink objtype gs) == "Coffee drank"

{- Check the following conditions: 1. The object the player wants to open is a door
                                   2. The player is in the Hall way
                                   3. The player has the key
                                   4. The player is caffeinated
                                   5. The player has a mask on
   If all the conditions are fulfilled, then unlock and open the door
-}
prop_open :: ObjectType -> GameData -> Bool
prop_open objtype gs | objtype /= Door                                              = snd(open objtype gs) == "You can't open this"
                     | location_id gs /= Hall                                       = snd(open objtype gs) == "There is no door here"
                     | key `notElem` inventory gs                                   = snd(open objtype gs) == "You don't have the key"
                     | not(caffeinated gs)                                          = snd(open objtype gs) == "You haven't drunk your coffee"
                     | not(maskon gs)                                               = snd(open objtype gs) == "You haven't put your mask on"
                     | otherwise                                                    = snd(open objtype gs) == "You've unlocked the door with the key"

{- Check the following conditions: 1. The object the player wants to wear is a mask
                                   2. The player has the mask (i.e. in the inventory)
   If all the conditions are fulfilled, then the player will wear the mask.

-}
prop_wear :: ObjectType -> GameData -> Bool
prop_wear objtype gs | objtype /= Mask                                              = snd (wear objtype gs) == "This item isn't wearable"
                     | carrying gs Mask                                             = snd (wear objtype gs) == "Mask on"
                     | otherwise                                                    = snd (wear objtype gs) == "You don't have a mask on you. Are you tripping?"

{- Check if the ObjectType is a switch. If the light is on and the location is the bedroom, light will be turned off.
   If the light is off and the location is the bedroom, the light will be turned on. Otherwise, it means that there is
   no light in the room.
-}
prop_press :: ObjectType -> GameData -> Bool
prop_press objtype gs | objtype /= Switch                                           = snd(press objtype gs) == "You can't press this"
                      | lighton gs && location_id gs == Bedroom                     = snd(press objtype gs) == "You've switched the lights off"
                      | location_id gs == Bedroom                                   = snd(press objtype gs) == "lights on, now you can explore the house"
                      | otherwise                                                   = snd(press objtype gs) == "There's no light switch in this room"

{-Test that if the object is in the inventory, the function inv will return a string telling the player that they are carryiong the object-}
prop_inv1 :: GameData -> Object  -> Bool 
prop_inv1 gs obj = snd (inv gs') == "You are carrying:\n" ++ obj_longname obj
       where gs' = gs{inventory = inventory gs ++ [obj]}

{-Test that if there's nothing in the inventory, the function inv should return "You aren't carrying anything"-}
prop_inv2 :: GameData -> Bool
prop_inv2 gs = snd (inv gs) == "You aren't carrying anything"

{- Check if quit is returning Bye bye -}
prop_quit :: GameData -> Bool 
prop_quit gs = snd (quit gs) == "Bye bye"

return []

-- finds all properties for testing and runs them
runTests = $quickCheckAll

main :: IO Bool
main = do putStrLn "Start Testing"
          runTests
