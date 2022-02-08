{-# LANGUAGE TemplateHaskell #-}
module Test where
import Test.QuickCheck
import Test.QuickCheck.All
import Data.Maybe
import World
import Actions
import World (Direction, Room)
import Actions (move)
import Data.Maybe (fromJust)

instance Arbitrary Object where
    arbitrary = oneof [return mug,
                       return fullmug,
                       return coffeepot,
                       return mask,
                       return key,
                       return switch,
                       return suspiciouscoffee]

instance Arbitrary ObjectID where
    arbitrary = oneof [return Mug,
                       return Cup,
                       return Coffee,
                       return Mask,
                       return Key,
                       return Switch,
                       return Door]

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
                           (1, retrun Quit)]

instance Arbitrary GameData where
    arbitrary = return state

-- Checking if RoomID type is returned when calling the move funciton. It also checks if "Nothing" is returned
       -- !!! RoomID ok?
prop_move :: Direction -> Room -> Bool
prop_move dir rm | isJust(move dir rm) = fromJust (move dir rm) `elem` RoomID
                 | otherwise = isNothing (move dir rm)

-- Checking if the ObjectID is actaully an object from the room
       -- !!! objectid to object?
prop_objectHere :: ObjectID  -> Room -> Bool
prop_objectHere objid rm = objectHere objid rm == (objid `elem` (objects rm))

-- Checking if is updated, with the object removed after calling the method
prop_removeObject :: ObjectID -> Room -> Bool
prop_removeObject objid rm =  objectHere objid (removeObject objid rm) == False

-- Checking if object is added to the room when calling addObject function.
prop_addObject :: Object -> Room -> Bool
prop_addObject obj rm = objectHere (obj_name obj) (addObject obj rm)

-- Checking if the object is from the object list
prop_findObj :: ObjectID -> [Object] -> Bool
prop_findObj obj ds = findObj obj ds `elem` ds

-- Check if the room is updated using the world data
       -- !!! check updated room
prop_updateRoom :: GameData -> RoomID -> Room -> Bool
prop_updateRoom gd rmid rm = let new_gd = updateRoom gd rmid rm
                             in getIndivRoom gd rmid == rm

-- Check if the carrying object is inside the inventory 
       -- !!!!Need objid to obj
prop_carrying :: GameData -> ObjectID -> Bool
prop_carrying gd objid = carrying gd objid == (objid `elem` inventory gd)

prop_addInv :: GameData -> ObjectID -> Bool
prop_addInv gd objid = (addInv gd objid) `elem` GameData

prop_removeInv :: GameData -> ObjectID -> Bool
prop_removeInv gd objid = (removeInv gd objid) `elem` GameData



return []
runTests = $quickCheckAll