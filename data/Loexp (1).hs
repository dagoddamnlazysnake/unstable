module Loexp where

import System.IO -- I/O

import Data.List
import Data.Char
import Data.Bool

type Exp = Int
type Time = Float --Ezt lehet én írtam át
type Level = Int
type Tier = Int
type Price = Int
type Horse = (Level, Tier)
type Coordinate = (Time, Price) --NEW

type ExpChart = [(Level,[Exp])]

type Parameters = (Price,Price,Exp,Time,Time,Price,Price,Level,Level,Tier,Tier)


main = do
    
    let profit_min = 50000000 :: Price
    let profit_max = 1000000000 :: Price
    
    let global_tick_exp = 500 :: Exp
    let global_time_bw_ticks = 10.4 :: Time
    
    let time_max = 0 :: Time -- set to 0 to accept all permutations
    
    let value_min = 0 :: Price -- set to 0 to accept all horses
    let value_max = 0 :: Price -- set to 0 to accept all horses
    let level_min = 5 :: Level
    let level_max = 30 :: Level
    let tier_min  = 1 :: Tier
    let tier_max  = 8 :: Tier
    
    
    let breedbest = ([(27,7),(27,7),(27,7),(27,7)],164203200,690655.6799999999)
                 -- ([(27,8),(27,8),(27,8),(27,8)],419126400,789286.7840000001)
    let absolutebest = ([(20,7),(20,7),(20,7),(20,7)],145152000,329361.76000000007)
    let assumed_best = breedbest
                    
    
    let params    = (
                     profit_min, profit_max, 
                     global_tick_exp, global_time_bw_ticks, 
                     time_max, 
                     value_min, value_max, 
                     level_min, level_max, 
                     tier_min, tier_max
                     )
                     
                     
    let expchart = [(1    ,[2605    ,5105    ,7610    ,10110    ,12615    ,15115    ,17620    ,20120]),
                    (2    ,[5120    ,10140    ,15160    ,20180    ,25200    ,30220    ,35240    ,40260]),
                    (3    ,[7670    ,15235    ,22805    ,30370    ,37940    ,45505    ,53075    ,60640]),
                    (4    ,[10260    ,20420    ,30580    ,40740    ,50900    ,61060    ,71220    ,81380]),
                    (5    ,[12915    ,25725    ,38540    ,51350    ,64165    ,76975    ,89790    ,102600]),
                    (6    ,[15640    ,31180    ,46720    ,62260    ,77800    ,93340    ,108880    ,124420]),
                    (7    ,[18460    ,36815    ,55175    ,73530    ,91890    ,110245    ,128605    ,146960]),
                    (8    ,[21380    ,42660    ,63940    ,85220    ,106500    ,127780    ,149060    ,170340]),
                    (9    ,[24425    ,48745    ,73070    ,97390    ,121715    ,146035    ,170360    ,194680]),
                    (10    ,[27600    ,55100    ,82600    ,110100    ,137600    ,165100    ,192600    ,220100]),
                    (11    ,[30930    ,61755    ,92585    ,123410    ,154240    ,185065    ,215895    ,246720]),
                    (12    ,[34420    ,68740    ,103060    ,137380    ,171700    ,206020    ,240340    ,274660]),
                    (13    ,[38095    ,76085    ,114080    ,152070    ,190065    ,228055    ,266050    ,304040]),
                    (14    ,[41960    ,83820    ,125680    ,167540    ,209400    ,251260    ,293120    ,334980]),
                    (15    ,[46040    ,91975    ,137915    ,183850    ,229790    ,275725    ,321665    ,367600]),
                    (16    ,[50340    ,100580    ,150820    ,201060    ,251300    ,301540    ,351780    ,402020]),
                    (17    ,[54885    ,109665    ,164450    ,219230    ,274015    ,328795    ,383580    ,438360]),
                    (18    ,[59680    ,119260    ,178840    ,238420    ,298000    ,357580    ,417160    ,476740]),
                    (19    ,[64750    ,129395    ,194045    ,258690    ,323340    ,387985    ,452635    ,517280]),
                    (20    ,[70100    ,140100    ,210100    ,280100    ,350100    ,420100    ,490100    ,560100]),
                    (21    ,[75755    ,151405    ,227060    ,302710    ,378365    ,454015    ,529670    ,605320]),
                    (22    ,[81720    ,163340    ,244960    ,326580    ,408200    ,489820    ,571440    ,653060]),
                    (23    ,[88020    ,175935    ,263855    ,351770    ,439690    ,527605    ,615525    ,703440]),
                    (24    ,[94660    ,189220    ,283780    ,378340    ,472900    ,567460    ,662020    ,756580]),
                    (25    ,[101665    ,203225    ,304790    ,406350    ,507915    ,609475    ,711040    ,812600]),
                    (26    ,[109040    ,217980    ,326920    ,435860    ,544800    ,653740    ,762680    ,871620]),
                    (27    ,[116810    ,233515    ,350225    ,466930    ,583640    ,700345    ,817055    ,933760]),
                    (28    ,[124980    ,249860    ,374740    ,499620    ,624500    ,749380    ,874260    ,999140]),
                    (29    ,[133575    ,267045    ,400520    ,533990    ,667465    ,800935    ,934410    ,1067880])]
    
    let horsedata = [ (lv, tier) | 
                        
                      lv <- [level_min..level_max], 
                      tier <- [tier_min..tier_max], 
                      
                      and [
                            getPrice (lv, tier) < profit_max,
                            isValuable value_min value_max (lv, tier),
                            or [
                                time_max == 0, 
                                time_max > getTimeToLevel (1,tier) expchart global_tick_exp global_time_bw_ticks lv
                               ],
                            getAngle(getTimeToLevel (1,tier) expchart global_tick_exp global_time_bw_ticks lv,getPrice(lv,tier)-(getPrice(lv,tier)-getPrice(0,tier))) >=45.0 --NEW
                          ]
                    ]      


    -- let expfort5lv5 = getExpForNextLevel (5,5) expchart
    -- let expfort1lv2 = getExpForLevel (1,1) expchart 2
    
    -- let time_t1lv1_t1lv2 = getTimeToNextLevel (1,1) expchart 500 10.4 
    -- let time_t1lv1_t1lv3 = getTimeToLevel (1,1) expchart 500 10.0 3 
    
    let all_permutations = [  
                            (
                             [h1,h2,h3,h4],
                             sum $ map getPrice [h1,h2,h3,h4], 
                             sum $ map (\(lv, tier) -> getTimeToLevel (1,tier) expchart global_tick_exp global_time_bw_ticks lv) [h1,h2,h3,h4]
                            )
                              
                            | h1 <- horsedata, 
                              h2 <- horsedata, 
                              h3 <- horsedata, 
                              h4 <- horsedata, 
                              
                              and $ map (isValuable value_min value_max) [h1,h2,h3,h4],
                              profit_min < (sum $ map getPrice [h1,h2,h3,h4]),
                              profit_max > (sum $ map getPrice [h1,h2,h3,h4]),
                              or [time_max == 0, 
                                  time_max > (sum $ map (\(lv, tier) -> getTimeToLevel (0,tier) expchart global_tick_exp global_time_bw_ticks lv) [h1,h2,h3,h4]) ]
                             
                            ]
                                             
    -- let all_prices = map (\(_,price,_) -> price) all_permutations
    -- let all_times  = map (\(_,_,time) -> time) all_permutations
    
    -- let expchart' = read (args !! 0) :: ExpChart
    
    -- putStrLn $ show time_t1lv1_t1lv3
    putStrLn "Running with the following parameters: "
    putStr "profit_min "
    putStrLn $ show profit_min
    putStr "profit_max "
    putStrLn $ show profit_max
    
    putStr "global_tick_exp "
    putStrLn $ show global_tick_exp
    putStr "global_time_bw_ticks "
    putStrLn $ show $ global_time_bw_ticks
    putStr "time_max " 
    putStrLn $ show $ time_max
    
    putStr "value_min "
    putStrLn $ show value_min
    putStr "value_max "
    putStrLn $ show value_max
    
    putStr "level_min "
    putStrLn $ show level_min
    putStr "level_max "
    putStrLn $ show level_max
    
    putStr "tier_min "
    putStrLn $ show tier_min
    putStr "tier_max "
    putStrLn $ show tier_max
    
    putStrLn "Comparison on price to time ratio."
    putStr "Assumed best: "
    putStrLn $ show assumed_best
    
    -- putStrLn $ show $ maximumBy (\data1 data2 -> comparison data1 data2) all_permutations
    
    -- let h = [(27,5),(27,5),(27,6),(27,7)]
    
    -- putStrLn $ show $ getPermutationData expchart global_tick_exp global_time_bw_ticks h
    
    -- putStrLn $ show $ all_permutations
    
    putStrLn $ show $ doEpicCalculation params expchart assumed_best all_permutations
    
    
toDegrees :: Floating a => a -> a --NEW
toDegrees rad = rad * 180 / pi

getAngle :: Coordinate -> Float --NEW
getAngle (t,p) = toDegrees $ atan ((fromIntegral p)/t)  

getExpForNextLevel :: Horse -> ExpChart -> Exp
getExpForNextLevel (lv, tier) e = snd(e !! (lv - 1)) !! (tier - 1)

getExpForLevel :: Horse -> ExpChart -> Level -> Exp
getExpForLevel h@(lv, tier) e desiredlevel
    | desiredlevel - lv == 0 = 0
    | otherwise              = (+) (getExpForNextLevel h e) (getExpForLevel (lv+1, tier) e desiredlevel)

getTimeToNextLevel :: Horse -> ExpChart -> Exp -> Time -> Time
getTimeToNextLevel h@(lv,tier) e tick_exp time_bw_ticks = (fromIntegral (getExpForNextLevel h e) / fromIntegral tick_exp)*time_bw_ticks

getTimeToLevel ::  Horse -> ExpChart -> Exp -> Time -> Level -> Time
getTimeToLevel _ _ _ _ 0 = 0.0
getTimeToLevel h@(lv,tier) e tick_exp time_bw_ticks desiredlevel
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
doEpicCalculation params@(_, _, _, _, _, _, _, _, lmax, _, tmax) e best (c@(current,curprice,curtime):perms) 
    | atEnd params current = maximumBy (\data1 data2 -> comparison data1 data2) [best, (current,curprice,curtime)]
    | otherwise            = doEpicCalculation params e (maximumBy (\data1 data2 -> comparison data1 data2) [best, (current,curprice,curtime)]) perms
        where
            atEnd :: Parameters -> [Horse] -> Bool
            atEnd (_, _, _, _, _, _, _, _, lmax, _, tmax) current = foldr (\(l,t) -> (&&) (and [l == lmax,t == tmax])) True current
            

