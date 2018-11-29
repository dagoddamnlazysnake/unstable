module Typestuff where

import Data.List
import Data.Char
import Data.Bool

type Gender = String
type Exp = Int
type Time = Float
type Level = Int
type Tier = Int
type Price = Int
type Horse = (Level, Tier)
type Coordinate = (Time, Price)

type ExpChart = [(Level,[Exp])]

type Parameters = (Price,Price,Exp,Time,Time,Price,Price,Level,Level,Tier,Tier)

type Breed = (Gender,Level,Tier)
type Breeding = (Horse,Horse,[Breed])

-- bad_encode :: String -> String
-- bad_encode [] = []
-- bad_encode (c1:c2:str) = (chr (mod (ord c2 + 31) (127))) : (chr (mod (ord c1 + 31) (127))) : (encode str)
-- bad_encode ('\n':str) = (chr (mod (ord 'Z' + 31) (127))) : (encode str)
-- bad_encode (c:str) = (chr (mod (ord c + 31) (127))) : (encode str)

encode :: String -> String
encode [] = []
encode ('\n':str) = encode $ 'Z' : str
encode (c1:c2:c3:str) = (chr $ (ord c3) + 1) : (chr $ (ord c1) + 3) : (chr $ (ord c2) + 2) : (encode str)
encode str = str

decode :: String -> String
decode [] = []
decode (c1:c2:c3:str) = (chr $ (ord c2) - 3) : (chr $ (ord c3) - 2) : (chr $ (ord c1) - 1) : (decode str)
decode ('Z':str) = decode $ '\n' : str
decode str = str

toDegrees :: Floating a => a -> a
toDegrees rad = rad * 180 / pi

slice :: Int -> Int -> [a] -> ([a],[a])
slice from len list = (take len $ drop from list, drop (from + len) list)

pNS :: (Eq a) => (Ord a) => Int -> [(Int,a)] -> [[(Int,a)]]
pNS 0 _  = []
pNS 1 hs = map (\h -> [h]) hs
pNS n hs = filter (\p -> length p == n) . map (nub . sort) $ concat $ map (\h -> (map (\hns -> h : hns) $ pNS (n-1) hs)) hs


