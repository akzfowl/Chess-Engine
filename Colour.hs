module Colour (Colour (Black, White), opponent) where

data Colour = Black | White deriving (Show, Eq)

opponent :: Colour -> Colour
opponent Black = White
opponent White = Black