{-
Functional parsing library from chapter 8 of Programming in Haskell,
Graham Hutton, Cambridge University Press, 2007.

Minor changes by Edwin Brady
-}

module Parsing where

import World
import Data.Char
import Control.Monad
import Control.Applicative hiding (many)

infixr 5 |||

{-
The monad of parsers
--------------------
-}

newtype Parser a              =  P (String -> [(a,String)])

instance Functor Parser where
   fmap f p = do p' <- p
                 return (f p')

instance Applicative Parser where
   pure = return
   f <*> a = do f' <- f
                a' <- a
                return (f' a')

instance Monad Parser where
   return v                   =  P (\inp -> [(v,inp)])
   p >>= f                    =  P (\inp -> case parse p inp of
                                               []        -> []
                                               [(v,out)] -> parse (f v) out)

instance Alternative Parser where
   empty = mzero
   p <|> q = p ||| q

instance MonadPlus Parser where
   mzero                      =  P (\inp -> [])
   p `mplus` q                =  P (\inp -> case parse p inp of
                                               []        -> parse q inp
                                               [(v,out)] -> [(v,out)])

{-
Basic parsers
-------------
-}

failure                       :: Parser a
failure                       =  mzero

item                          :: Parser Char
item                          =  P (\inp -> case inp of
                                               []     -> []
                                               (x:xs) -> [(x,xs)])

parse                         :: Parser a -> String -> [(a,String)]
parse (P p) inp               =  p inp

{-
Choice
------
-}

(|||)                         :: Parser a -> Parser a -> Parser a
p ||| q                       =  p `mplus` q

{-
Derived primitives
------------------
-}

sat                           :: (Char -> Bool) -> Parser Char
sat p                         =  do x <- item
                                    if p x then return x else failure

digit                         :: Parser Char
digit                         =  sat isDigit

lower                         :: Parser Char
lower                         =  sat isLower

upper                         :: Parser Char
upper                         =  sat isUpper

letter                        :: Parser Char
letter                        =  sat isAlpha

alphanum                      :: Parser Char
alphanum                      =  sat isAlphaNum

char                          :: Char -> Parser Char
char x                        =  sat (== x)

string                        :: String -> Parser String
string []                     =  return []
string (x:xs)                 =  do char x
                                    string xs
                                    return (x:xs)

many                          :: Parser a -> Parser [a]
many p                        =  many1 p ||| return []

many1                         :: Parser a -> Parser [a]
many1 p                       =  do v  <- p
                                    vs <- many p
                                    return (v:vs)

ident                         :: Parser String
ident                         =  do x  <- lower
                                    xs <- many alphanum
                                    return (x:xs)

nat                           :: Parser Int
nat                           =  do xs <- many1 digit
                                    return (read xs)

int                           :: Parser Int
int                           =  do char '-'
                                    n <- nat
                                    return (-n)
                                  ||| nat

space                         :: Parser ()
space                         =  do many (sat isSpace)
                                    return ()
{-
Ignoring spacing
----------------
-}

token                         :: Parser a -> Parser a
token p                       =  do space
                                    v <- p
                                    space
                                    return v

identifier                    :: Parser String
identifier                    =  token ident

natural                       :: Parser Int
natural                       =  token nat

integer                       :: Parser Int
integer                       =  token int

symbol                        :: String -> Parser String
symbol xs                     =  token (string xs)


{-
Direction parsers
-------------
-}
parseDirection                :: Parser Direction 
parseDirection                =  do symbol "north"
                                    return North
                             ||| do symbol "south"
                                    return South 
                             ||| do symbol "east"
                                    return East 
                             ||| do symbol "west"
                                    return West 
                             ||| do symbol "in"
                                    return In 
                             ||| do symbol "out"
                                    return Out 

{-
Object parsers
-------------
-}
parseObject                  :: Parser ObjectID 
parseObject                 =  do symbol "mug"
                                  return Mug
                           ||| do symbol "coffee"
                                  return Coffee
                           ||| do symbol "switch"
                                  return Switch
                           ||| do symbol "key"
                                  return Key
                           ||| do symbol "door"
                                  return Door
                           ||| do symbol "mask"
                                  return Mask

{-
Command parsers
-------------
-}                                  
parseGo :: Parser Command 
parseGo = do symbol "go"
             Go <$> parseDirection

parseGet :: Parser Command  
parseGet = do symbol "get"
              Get <$> parseObject

parseDrop :: Parser Command  
parseDrop = do symbol "drop"
               Put <$> parseObject
              
parsePour :: Parser Command  
parsePour = do symbol "pour"
               Pour <$> parseObject

parseExamine :: Parser Command  
parseExamine = do symbol "examine"
                  Examine <$> parseObject
        
parseDrink :: Parser Command  
parseDrink = do symbol "drink"
                Drink <$> parseObject

parseOpen :: Parser Command  
parseOpen = do symbol "open"
               Open <$> parseObject

parseWear :: Parser Command  
parseWear = do symbol "wear"
               Wear <$> parseObject
               
parseUse :: Parser Command
parseUse = do symbol "use"
              Use <$> parseObject
              
parsePress :: Parser Command 
parsePress = do symbol "press"
                Press <$> parseObject

parseInventory :: Parser Command 
parseInventory = do symbol "inventory"
                    return Inventory

parseQuit :: Parser Command 
parseQuit = do symbol "quit"
               return Quit 

parseCommand :: Parser Command 
parseCommand = do parseGo 
              ||| parseGet
              ||| parseDrop
              ||| parsePour
              ||| parseExamine
              ||| parseDrink
              ||| parseOpen
              ||| parseWear
              ||| parseUse
              ||| parsePress
              ||| parseInventory
              ||| parseQuit

runParser :: String -> Maybe Command 
runParser xs = case parse parseCommand xs of
               [(cmd, "")] -> Just cmd
               _ -> Nothing


parseRoom :: [Char] -> RoomID
parseRoom str |str == "Bedroom" = Bedroom
              |str =="Kitchen" = Kitchen
              |str == "Hall" = Hall
              |str == "LivingRoom" = LivingRoom

remBrack (x:xs) = init xs
remBrack [] = ""

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen pred str = case dropWhile pred str of
                           "" -> []
                           str' -> w : wordsWhen pred str''
                                   where (w, str'') = break pred str'

splitInv str = wordsWhen (==',') (remBrack str)

parseInv :: [[Char]] -> [Object]
parseInv (x:xs) |x == "a coffee mug" = mug:parseInv xs
                |x == "a full coffee mug" = fullmug:parseInv xs
                |x == "a pot of coffee" = coffeepot:parseInv xs
                |x == "a piece of mask" = mask:parseInv xs
                |x == "a key for a door" = key:parseInv xs
                |x == "a switch for the light" = switch:parseInv xs
parseInv [] = []

parseBool :: [Char] -> Bool
parseBool str = str == "True"
