{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module World where
import Data.Maybe ( fromJust )


instance Show Object where
   show obj = obj_longname obj
   
instance Show Room where
    show (Room desc exits objs) = desc ++ "\n" ++ concatMap exit_desc exits ++
                                  showInv objs
       where showInv [] = ""
             showInv xs = "\nYou can see: " ++ showInv' xs
             showInv' [x] = show x
             showInv' (x:xs) = show x ++ ", " ++ showInv' xs
                                  

instance Show GameData where
    show gd = show (getRoomData gd)
    

data Object = Obj { obj_name :: ObjectType,
                    obj_longname :: String,
                    obj_desc :: String }
    deriving Eq

data Exit = Exit { exit_dir :: Direction,
                   exit_desc :: String,
                   room :: RoomID }
   deriving Eq

data Room = Room { room_desc :: String,
                   exits :: [Exit],
                   objects :: [Object] }
   deriving Eq

data GameData = GameData { location_id :: RoomID, -- where player is
                           world :: [(RoomID, Room)],
                           inventory :: [Object], -- objects player has
                           caffeinated :: Bool, -- coffee is drunk
                           lighton :: Bool, -- the light is on
                           maskon :: Bool, -- the mask is on
                           poisoned :: Bool, -- drank poison coffee
                           finished :: Bool -- set to True at the end
                         }


won :: GameData -> Bool
won gd = location_id gd == Street

data Direction = North | South | East | West | Out
    deriving (Eq, Show)
    
data ObjectType = Mug | Coffee | Mask | Key | Switch | Door 
    deriving (Eq, Show)

data RoomID = Bedroom | Kitchen | LivingRoom | DinningRoom | Hall | Street
    deriving (Eq, Show)

-- Things which do something to an object and update the game state
data Command = Go Direction
             | Get ObjectType
             | Put ObjectType
             | Examine ObjectType
             | Pour ObjectType
             | Drink ObjectType
             | Open ObjectType
             | Wear ObjectType
             | Press ObjectType
             | Inventory
             | Quit
             deriving Show
             

-- Things which just update the game state
type Action = GameData -> (GameData, String)

mug, fullmug, weirdcoffee, coffeepot, mask, key, switch, suspiciouspot :: Object
mug            = Obj Mug "a coffee mug" "A coffee mug"
fullmug        = Obj Mug "a full coffee mug" "A coffee mug containing freshly brewed coffee"
weirdcoffee    = Obj Mug "a mug filled with suspicious coffee" "A mug filled with suspicious coffee"
coffeepot      = Obj Coffee "a pot of coffee" "A pot containing freshly brewed coffee"
suspiciouspot  = Obj Coffee "a suspicious coffee pot" "A pot of suspicious coffee. You better drop it"
mask           = Obj Mask "a piece of mask" "A piecee of mask protect people from catching COVID"
key            = Obj Key "a key for a door" "A myterious key"
switch         = Obj Switch "a switch for the light" "A switch for the light"


bedroom, kitchen, hall, street, livingroom, dinningroom :: Room

bedroom = Room "You are in your bedroom.There's a light switch on the wall"
               [Exit North "To the north is a kitchen. " Kitchen]
               [mug]

kitchen = Room "You are in the kitchen."
               [Exit South "To the south is your bedroom. " Bedroom,
                Exit West "To the west is a hallway. " Hall,
                Exit North "To the north is a dinning room" DinningRoom]
               [suspiciouspot]

hall = Room "You are in the hallway. The front door is closed. "
            [Exit East "To the east is a kitchen. " Kitchen,
             Exit North "To the north is a living room." LivingRoom]
            []

livingroom = Room "You are in the living room."
            [Exit South "To the south is a hallway. " Hall,
             Exit East "To the east is a living room. " LivingRoom]
            [key,mask]

dinningroom = Room "You are in the dinning room."
            [Exit South "To the south is a kitchen. " Kitchen,
             Exit West "To the west is a living room" LivingRoom]
            [coffeepot, mug]  

street = Room "You have made it out of the house."
              []
              []

gameworld :: [(RoomID, Room)]
gameworld = [(Bedroom, bedroom),
             (Kitchen, kitchen),
             (Hall, hall),
             (LivingRoom, livingroom),
             (DinningRoom, dinningroom),
             (Street, street)]

initState :: GameData
initState = GameData Bedroom gameworld [] False False False False False

{- Return the room the player is currently in. -}

getRoomData :: GameData -> Room
getRoomData gd = fromJust (lookup (location_id gd) (world gd))
