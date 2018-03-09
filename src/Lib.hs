module Lib
    ( mapMessage
    , generateKey
    , prepare
    ) where

import System.Random (mkStdGen)
import System.Random.Shuffle (shuffle')
import Data.List (takeWhile, dropWhile, elemIndex, last, zipWith, elem)
import Data.Maybe (fromJust)
import Data.Char (toLower)

data Suit = Club | Diamond | Heart | Spade deriving (Eq, Show)

data Card = NormalCard Suit Int | JokerA | JokerB deriving (Eq, Show)

type Hand = [Card]

letters = ['a'..'z']
suits = [Club, Diamond, Heart, Spade]
ranks = [1..13]
deck = JokerA:JokerB:[NormalCard suit rank | suit <- suits, rank <- ranks]
deckLength = length deck
shuffledDeck = shuffle' deck deckLength $ mkStdGen deckLength

mapMessage :: (Int -> Int -> Int) -> String -> String -> String
mapMessage f message key = zipWith (calcLetters f) message key

generateKey :: String -> String
generateKey message = generateKey' (length message) shuffledDeck ""

generateKey' :: Int -> Hand -> String -> String
generateKey' len previousDeck previousKey
  | (length previousKey) == len = previousKey
  | otherwise =
    let
      currentDeck = nextDeck previousDeck
      card = currentDeck !! (value $ head currentDeck)
    in
      if
        (card == JokerA) || (card == JokerB)
      then
        generateKey' len currentDeck previousKey
      else
        generateKey' len currentDeck ((letters !! ((value card) `mod` 26)):previousKey)

nextDeck :: Hand -> Hand
nextDeck = switchByLast . switchTopBottom . moveJokerB . moveJokerA

moveJokerA :: Hand -> Hand
moveJokerA deck =
  let
    top = takeWhile (/= JokerA) deck
    bottom = dropWhile (/= JokerA) deck
  in
    if
      (length bottom) == 1
    then
      (head top):JokerA:(tail top)
    else
      top ++ ((head $ tail bottom):JokerA:(tail $ tail bottom))

moveJokerB :: Hand -> Hand
moveJokerB deck =
  let
    top = takeWhile (/= JokerB) deck
    bottom = dropWhile (/= JokerB) deck
  in
    case bottom of
      [JokerB] -> (take 2 top) ++ (JokerB:(drop 2 top))
      [JokerB, bottomLast] -> (head top):JokerB:(tail top) ++ [bottomLast]
      otherwise -> top ++ (take 2 $ tail bottom) ++ (JokerB:(drop 3 bottom))

switchTopBottom :: Hand -> Hand
switchTopBottom deck =
  let
    indexA = findIndex JokerA deck
    indexB = findIndex JokerB deck
    indexMin = min indexA indexB
    indexMax = max indexA indexB
    top = take indexMin deck
    bottom = drop (indexMax + 1) deck
    middle = take (indexMax - indexMin + 1) $ drop indexMin deck
  in
    bottom ++ middle ++ top

switchByLast :: Hand -> Hand
switchByLast deck =
  let
    lastCard = last deck
    step = value lastCard
    top = take step deck
    middle = take ((length deck) - step - 1) $ drop step deck
    bottom = [lastCard]
  in
    middle ++ top ++ bottom

value :: Card -> Int
value JokerA = 53
value JokerB = 53
value (NormalCard Club rank) = rank
value (NormalCard Diamond rank) = rank + 13
value (NormalCard Heart rank) = rank + 26
value (NormalCard Spade rank) = rank + 39

calcLetters :: (Int -> Int -> Int) -> Char -> Char -> Char
calcLetters f a b =
  let
    indexA = findIndex a letters
    indexB = findIndex b letters
  in
    letters !! ((f indexA indexB) `mod` 26)

findIndex :: Eq a => a -> [a] -> Int
findIndex x xs = fromJust $ elemIndex x xs

prepare :: String -> String
prepare = (map toLower) . (filter (`elem` letters))
