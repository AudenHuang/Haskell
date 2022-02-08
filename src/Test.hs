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

-- Checking if RoomID type is returned when calling the move funciton. TODO: Need to check what happens when "Nothing" is returned
prop_move :: Direction -> Room -> Bool
prop_move dir rm = fromJust (move dir rm) `elem` RoomID || (move dir rm) == Nothing

-- Checking if Room type is returned when calling removeObject function.
prop_removeObject :: ObjectID -> Room -> Bool
prop_removeObject obj rm = (removeObject obj rm) `elem` Room

-- Checking if Room type is returned when calling addObject function.
prop_addObject :: Object -> Room -> Bool
prop_addObject obj rm = (addObject obj rm) `elem` Room

prop_findObj :: ObjectID -> [Object] -> Bool
prop_findObj obj ds = (findObj obj ds) `elem` Object

prop_updateRoom :: GameData -> RoomID -> Room -> Bool
prop_updateRoom gd rmid rm = (updateRoom gd rmid rm) `elem` GameData

prop_addInv :: GameData -> ObjectID -> Bool
prop_addInv gd objid = (addInv gd objid) `elem` GameData

prop_removeInv :: GameData -> ObjectID -> Bool
prop_removeInv gd objid = (removeInv gd objid) `elem` GameData

return []
runTests = $quickCheckAll