module Lib
    ( encode
    ) where

import System.Random
import System.Random.Shuffle

data Suit = Club | Diamond | Heart | Spade deriving Show

data Card = NormalCard Suit Int | JokerA | JokerB deriving Show

suits = [Club, Diamond, Heart, Spade]
ranks = [1..13]
deck = JokerA:JokerB:[NormalCard suit rank | suit <- suits, rank <- ranks]
deckLength = length deck

encode :: String -> [Card]
encode message = shuffle' deck deckLength $ mkStdGen deckLength
