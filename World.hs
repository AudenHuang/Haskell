module World where

data Object = Obj { obj_name :: String,
                    obj_longname :: String,
                    obj_desc :: String }
   deriving Eq

instance Show Object where
   show obj = obj_longname obj

data Exit = Exit { exit_dir :: String,
                   exit_desc :: String,
                   room :: String }
   deriving Eq

data Room = Room { room_desc :: String,
                   exits :: [Exit],
                   objects :: [Object] }
   deriving Eq

data GameData = GameData { location_id :: String, -- where player is
                           world :: [(String, Room)],
                           inventory :: [Object], -- objects player has
                           poured :: Bool, -- coffee is poured
                           caffeinated :: Bool, -- coffee is drunk
                           finished :: Bool -- set to True at the end
                         }

won :: GameData -> Bool
won gd = location_id gd == "street"

data Direction = North | South | East | West | In | Out
    deriving (Eq, Show)
    
data ObjectID = Mug | Coffee | Mask | Key | Switch | Door 
    deriving (Eq, Show)

data RoomID = Bedroom | Kitchen | LivingRoom | Hall | Street
    deriving (Eq, Show)

instance Show Room where
    show (Room desc exits objs) = desc ++ "\n" ++ concatMap exit_desc exits ++
                                  showInv objs
       where showInv [] = ""
             showInv xs = "\n\nYou can see: " ++ showInv' xs
             showInv' [x] = show x
             showInv' (x:xs) = show x ++ ", " ++ showInv' xs
                                  

instance Show GameData where
    show gd = show (getRoomData gd)

-- Things which do something to an object and update the game state
data Action  = Go Direction
             | Get ObjectID
             | Put ObjectID
             | Examine ObjectID
             | Pour ObjectID
             | Drink ObjectID
             | Open ObjectID
             | Wear ObjectID
             | Use ObjectID
             | Press ObjectID
             | Inventory
             | Quit
             deriving Show

-- Things which just update the game state
type Move = GameData -> (GameData, String)

mug, fullmug, coffeepot, mask, key, switch :: Object
mug       = Obj Mug "a coffee mug" "A coffee mug"
fullmug   = Obj Mug "a full coffee mug" "A coffee mug containing freshly brewed coffee"
coffeepot = Obj Coffee "a pot of coffee" "A pot containing freshly brewed coffee"
mask      = Obj Mask "a piece of mask" "A piecee of mask protect people from catching COVID"
key       = Obj Key "a key for a door" "A myterious key"
switch    = Obj Switch "a switch for the light"

bedroom, kitchen, hall, street, living room :: Room

bedroom = Room "You are in your bedroom."
               [Exit North "To the north is a kitchen. " Kitchen]
               [mug, switch]

kitchen = Room "You are in the kitchen."
               [Exit South "To the south is your bedroom. " Bedroom,
                Exit West "To the west is a hallway. " Hall]
               [coffeepot]

hall = Room "You are in the hallway. The front door is closed. "
            [Exit East "To the east is a kitchen. " Kitchen,
             Exit North "To the north is a living room." LivingRoom]
            [mask]

livingroom = Room "You are in the living room. There's a key on the table."
            [Exit South "To the south is a hallway. " Hall]
            [key]
            
-- New data about the hall for when we open the door

openedhall = "You are in the hallway. The front door is open. "
openedexits = [Exit East "To the east is a kitchen. " Kitchen,
               Exit Out "You can go outside. " Street]

street = Room "You have made it out of the house."
              [Exit In "You can go back inside if you like. " Hall]
              []

gameworld = [(Bedroom, bedroom),
             (Kitchen, kitchen),
             (Hall, hall),
             (LivingRoom, livingroom),
             (Street, street)]

initState :: GameData
initState = GameData Bedroom gameworld [] False False False

{- Return the room the player is currently in. -}

getRoomData :: GameData -> Room
getRoomData gd = fromJust (lookup (location_id gd) (world gd))
