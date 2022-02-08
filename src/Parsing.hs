{-
Functional parsing library from chapter 8 of Programming in Haskell,
Graham Hutton, Cambridge University Press, 2007.

Minor changes by Edwin Brady
-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

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
                             ||| do symbol "out"
                                    return Out 

{-
Object parsers
-------------
-}
parseObject                  :: Parser ObjectType 
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
                           ||| do symbol "cup"
                                  return Cup

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
              ||| parsePress
              ||| parseInventory
              ||| parseQuit
              
{-Given an imputted string from the save file, the appropriate RoomID is returned-}
parseRoom :: [Char] -> RoomID
parseRoom str |str == "Bedroom" = Bedroom
              |str == "Kitchen" = Kitchen
              |str == "Hall" = Hall
              |str == "LivingRoom" = LivingRoom
              |str == "DinningRoom" = DinningRoom

{-Given the inputted list representation from the save file, a list is generated removing the '[' and ']' characters-}
remBrack (x:xs) = init xs
remBrack [] = ""

{-Splits a string on a given char predicate, from https://stackoverflow.com/questions/4978578/how-to-split-a-string-in-haskell-}
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen pred str = case dropWhile pred str of
                           "" -> []
                           str' -> w : wordsWhen pred str''
                                   where (w, str'') = break pred str'

{-calls wordsWhen on the comma character to split the list representation into an actual list-}
splitInv str = wordsWhen (==',') (remBrack str)

{-Given save data for inventory objects, the appropriate object is returned-}
parseInv :: [[Char]] -> [Object]
parseInv (x:xs) |x == "a coffee mug" = mug:parseInv xs
                |x == "a full coffee mug" = fullmug:parseInv xs
                |x == "a pot of coffee" = coffeepot:parseInv xs
                |x == "a piece of mask" = mask:parseInv xs
                |x == "a key for a door" = key:parseInv xs
                |x == "a cup filled with suspicious coffee" = suspiciouscoffee:parseInv xs
                |x == "a switch for the light" = switch:parseInv xs
parseInv [] = []

{-Parses booleans from a string (for parsing save data)-}
parseBool :: [Char] -> Bool
parseBool str = str == "True"

runParser :: String -> Maybe Command 
runParser xs = case parse parseCommand xs of
               [(cmd, "")] -> Just cmd
               _ -> Nothing
