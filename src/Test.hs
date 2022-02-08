{-# LANGUAGE TemplateHaskell #-}
module Main where
import Test.QuickCheck
import Test.QuickCheck.All
import Data.Maybe
import World
import Actions
import World (Direction, Room, ObjectType)
import Actions (move)
import Data.Maybe (fromJust)

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

-- Checking if RoomID type is returned when calling the move funciton. It also checks if "Nothing" is returned
prop_move :: GameData -> Direction -> Room -> Bool
prop_move gd dir rm | isJust(move dir rm) = fromJust (move dir rm) `elem` map fst (world gd)
                    | otherwise = isNothing (move dir rm)

-- Checking if the ObjectID is actaully an object from the room
prop_objectHere :: ObjectType  -> Room -> Bool
prop_objectHere objtype rm = objectHere objtype rm == (objtype `elem` map obj_name (objects rm))

-- Checking if is updated, with the object removed after calling the method
prop_removeObject :: ObjectType -> Room -> Bool
prop_removeObject objtype rm =  objectHere objtype (removeObject objtype rm) == False

-- Checking if object is added to the room when calling addObject function.
prop_addObject :: Object -> Room -> Bool
prop_addObject obj rm = objectHere (obj_name obj) (addObject obj rm)

-- Checking if the object is from the object list
       -- !!!
prop_findObj :: ObjectType  -> [Object] -> Bool
prop_findObj objtype ds = findObj objtype ( ds++ [objtype]) `elem` ds

-- Check if the room is updated using the world data
prop_updateRoom :: GameData -> RoomID -> Room -> Bool
prop_updateRoom gd rmid rm = let new_gd = updateRoom gd rmid rm
                             in getIndivRoom new_gd rmid == rm

-- Check if the carrying object is inside the inventory
prop_carrying :: GameData -> ObjectType  -> Bool
prop_carrying gd objtype = carrying gd objtype == (findObj objtype (inventory gd) `elem` inventory gd)

-- Check if the player is carrying the object that is just added to inventory
       -- !!!
prop_addInv :: GameData -> ObjectType  -> Bool
prop_addInv gd objtype = carrying (addInv gd objtype) objtype

-- Check if the player is NOT carrying the object that is just removed from the inventory
prop_removeInv :: GameData -> ObjectType -> Bool
prop_removeInv gd objtype = carrying (removeInv gd objtype) objtype == False



return []
runTests = $quickCheckAll

main :: IO Bool
main = do putStrLn "Start Testing"
          runTests
