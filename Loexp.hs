module Loexp where

import System.IO -- I/O
import Typestuff as T

import Data.List
import Data.Char
import Data.Bool


getAngle :: Coordinate -> Float
getAngle (t,p) = T.toDegrees $ atan ((fromIntegral p)/t)

getExpForNextLevel :: Horse -> ExpChart -> Exp
getExpForNextLevel (lv, tier) e = snd(e !! (lv - 1)) !! (tier - 1)

getExpForLevel :: Horse -> ExpChart -> Level -> Exp
getExpForLevel h@(lv, tier) e desiredlevel
    | desiredlevel - lv == 0 = 0
    | desiredlevel - lv == 1 = getExpForNextLevel h e
    | otherwise              = (+) (getExpForNextLevel h e) (getExpForLevel (lv+1, tier) e desiredlevel)

getTimeToNextLevel :: Horse -> ExpChart -> Exp -> Time -> Time
getTimeToNextLevel h@(lv,tier) e tick_exp time_bw_ticks = (fromIntegral (getExpForNextLevel h e) / fromIntegral tick_exp)*time_bw_ticks

getTimeToLevel ::  Horse -> ExpChart -> Exp -> Time -> Level -> Time
getTimeToLevel _ _ _ _ 0 = 0.0
getTimeToLevel h@(lv,tier) e tick_exp time_bw_ticks desiredlevel
    | desiredlevel - lv == 0 = 0.0
    | desiredlevel - lv == 1 = getTimeToNextLevel h e tick_exp time_bw_ticks
    | otherwise              = (+) (getTimeToNextLevel h e tick_exp time_bw_ticks) (getTimeToLevel (lv+1,tier) e tick_exp time_bw_ticks desiredlevel)

    
getPrice :: Horse -> Price
getPrice (lv, tier) = priceAtLv0 tier + lv * (pricePerLevel tier)

isValuable :: Price -> Price -> Horse ->  Bool
isValuable min max h = or [min == 0,max == 0,and [getPrice h > min, getPrice h < max]]

getPermutationData :: ExpChart -> Exp -> Time -> [Horse] -> ([Horse],Price,Time)
getPermutationData e tick_exp time_bw_ticks h = (
                                                 h,
                                                 sum $ map getPrice h, 
                                                 sum $ map (\(lv, tier) -> getTimeToLevel (1,tier) e tick_exp time_bw_ticks lv) h
                                                )


horse_pNS :: Int -> [Horse] -> [[Horse]]
horse_pNS _ [] = []
horse_pNS 0 _  = []
horse_pNS n hs = map (map snd) $ T.pNS n $ zip [1..] hs
    
priceAtLv0 :: Tier -> Price
priceAtLv0 1 = 10000
priceAtLv0 2 = 15000
priceAtLv0 3 = 45000
priceAtLv0 4 = 135000
priceAtLv0 5 = 270000
priceAtLv0 6 = 3240000
priceAtLv0 7 = 22680000
priceAtLv0 8 = 68040000

pricePerLevel :: Tier -> Price
pricePerLevel 1 = 900
pricePerLevel 2 = 1200
pricePerLevel 3 = 3150
pricePerLevel 4 = 8100
pricePerLevel 5 = 13500
pricePerLevel 6 = 129600
pricePerLevel 7 = 680400
pricePerLevel 8 = 1360800


comparison :: ([Horse],Price,Time) -> ([Horse],Price,Time) -> Ordering
comparison (_,price1,time1) (_,price2,time2) = compare ((fromIntegral price1) / time1) ((fromIntegral price2) / time2)

doEpicCalculation :: Parameters -> ExpChart -> ([Horse],Price,Time) -> [([Horse],Price,Time)] -> ([Horse],Price,Time)
doEpicCalculation _ _ best [] = best
doEpicCalculation params@(_, _, _, _, _, _, _, _, lmax, _, tmax) e best@(bh,_,_) (c@(current,curprice,curtime):perms) 
    | atEnd params current = if length bh == 0 then c else maximumBy (\data1 data2 -> comparison data1 data2) [best, (current,curprice,curtime)]
    | otherwise            = doEpicCalculation params e (if length bh == 0 then c else (maximumBy (\data1 data2 -> comparison data1 data2) [best, (current,curprice,curtime)])) perms
        where
            atEnd :: Parameters -> [Horse] -> Bool
            atEnd (_, _, _, _, _, _, _, _, lmax, _, tmax) current = foldr (\(l,t) -> (&&) (and [l == lmax,t == tmax])) True current
            

doEpicCalculation' :: Parameters -> ExpChart -> [([Horse],Price,Time)] -> ([Horse],Price,Time)
doEpicCalculation' _ _ [] = ([],0,0)
doEpicCalculation' _ _ [c] = c
doEpicCalculation' params@(_, _, _, _, _, _, _, _, lmax, _, tmax) e (c@(current,curprice,curtime):c2:perms) = maximumBy (\data1 data2 -> comparison data1 data2) [(current,curprice,curtime), doEpicCalculation' params e (c2:perms)]


-- permEnumCalc :: Parameters -> ExpChart -> ([Horse],Price,Time) -> [([Horse],Price,Time)] -> ([Horse],Price,Time)
-- permEnumCalc _ _ _ [] = ([],0,0.0)
-- permEnumCalc params expchart best perm@[p] = doEpicCalculation params expchart best perm
-- permEnumCalc params expchart best (perm:perms) = doEpicCalculation params expchart (doEpicCalculation params expchart best perms) [perm]


