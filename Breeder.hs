module Breeder where

import System.IO -- I/O
import Typestuff

import Data.List
import Data.Char
import Data.Bool



getBestBreeding :: Tier -> Level -> Level -> [Breeding] -> Breeding
getBestBreeding _ _ _ [] = ((0,0),(0,0),[])
getBestBreeding tier lvmin lvmax breedings = minimumBy (\breeding1 breeding2 -> compare (chanceForLowerTier breeding1) (chanceForLowerTier breeding2)) $ filter (\((ml,_),(fl,_),_) -> and [ml >= lvmin,fl >= lvmin,ml <= lvmax,fl <= lvmax]) breedings
    where 
        chanceForLowerTier :: Breeding -> Int
        chanceForLowerTier (_,_,breed) = foldr (\(_,t,p) -> (+) (if t < tier then p else 0)) 0 breed



-- -- MONADIC SHITS -- --  

-- <- let helyett mert readFile :: String -> IO String 
-- es nem tudunk IO * cuccal dolgozni
-- de <- "lecsap" egy "monadic levelt"
-- return pedig visszarakja, szal mainben IO
-- ha megertetted torold ezeket a kommenteket pls  
        
getBreedData :: (Tier,Tier) -> IO [Breeding]
getBreedData (t1,t2) = 
    do
        raw_breed_data <- readFile (concat ["breed/", show t1, show t2, ".txt"]) 
        let formatted_breed_data = read (if last raw_breed_data == '\n' then (reverse $ drop 1 $ reverse raw_breed_data) else raw_breed_data) :: [Breeding]
        return formatted_breed_data
                            

renderBreedData :: (Tier,Tier) -> (Level,Level) -> IO Breeding
renderBreedData (t1,t2) (minlv,maxlv) =
    do
        formatted_breed_data <- getBreedData (t1,t2)
        let bestbreed = getBestBreeding t1 minlv maxlv formatted_breed_data
        return bestbreed