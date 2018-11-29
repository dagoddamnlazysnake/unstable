module Main where
-- v0.3.1 - 2018.10.08. - Slicing, Ignorable assumed_best
-- TODO resovle the fuckall of if_then_else logic

import System.IO -- I/O

import Data.List
import Data.Char
import Data.Bool

import Breeder as B
import Loexp as L
import Typestuff as T

main = do
    
    let profit_min = 100000000 :: Price
    let profit_max = 200000000 :: Price
    
    let global_tick_exp = 500 :: Exp
    let global_time_bw_ticks = 10.4 :: Time
    
    let time_max  = 0 :: Time -- set to 0 to accept all permutations
    
    let value_min = 0 :: Price -- set to 0 to accept all horses
    let value_max = 0 :: Price -- set to 0 to accept all horses
    let level_min = 27 :: Level
    let level_max = 27 :: Level
    let tier_min  = 4 :: Tier
    let tier_max  = 7 :: Tier
    
    let angle     = 60.0 :: Float
    let slice     = 100000 :: Int
    
    let dummybest = ([],0,0.0)
    -- let dummybest = ([(30,1),(30,1),(30,1),(30,1)],1,100000000.0)
    let breedbest = ([(27,7),(27,7),(27,7),(27,7)],164203200,690655.6799999999)
                 -- ([(27,8),(27,8),(27,8),(27,8)],419126400,789286.7840000001)
    let absolutebest = ([(20,7),(20,7),(20,7),(20,7)],145152000,329361.76000000007)
    let assumed_best = dummybest
    
    let encode = id
    -- let encode = T.decode . T.encode
    -- let encode = T.encode
                    
    
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
    
    -- TESTEN peNIS
    let my_pen_is_in_a_goat = horse_pNS 4 [(1,1),(1,1),(20,5),(3,1),(20,4),(30,8)]
    let touch_the_cow = map (getPermutationData expchart global_tick_exp global_time_bw_ticks) my_pen_is_in_a_goat
    let do_it_now = encode $ (show touch_the_cow) ++ "\n"
    
    putStr do_it_now
    -- TESETN END
    
    let horsedata = [ (lv, tier) | 
                        
                      lv <- [level_min..level_max], 
                      tier <- [tier_min..tier_max], 
                      
                      and [
                            L.getPrice (lv, tier) < profit_max,
                            L.isValuable value_min value_max (lv, tier),
                            or [
                                time_max == 0, 
                                time_max > L.getTimeToLevel (1,tier) expchart global_tick_exp global_time_bw_ticks lv
                               ],
                            angle <= L.getAngle (
                                        L.getTimeToLevel (1,tier) expchart global_tick_exp global_time_bw_ticks lv, 
                                        (-) (L.getPrice (lv,tier)) (L.priceAtLv0 tier)
                                    )
                            
                          ]
                    ]
                    
    putStr $ encode $ (show horsedata) ++ "\n"
    putStr $ encode $ (show $ length horsedata) ++ "\n"


    -- let expfort5lv5 = getExpForNextLevel (5,5) expchart
    -- let expfort1lv2 = getExpForLevel (1,1) expchart 2
    
    -- let time_t1lv1_t1lv2 = getTimeToNextLevel (3,4) expchart 500 10.4 
    -- let time_t1lv1_t1lv3 = getTimeToLevel (3,4) expchart 500 10.0 15 
    
    let all_permutations = [  
                            (
                             [h1,h2,h3,h4],
                             sum $ map getPrice [h1,h2,h3,h4], 
                             sum $ map (\(lv, tier) -> L.getTimeToLevel (1,tier) expchart global_tick_exp global_time_bw_ticks lv) [h1,h2,h3,h4]
                            )
                              
                            | h1 <- horsedata, 
                              h2 <- horsedata, 
                              h3 <- horsedata, 
                              h4 <- horsedata, 
                              
                              and $ map (L.isValuable value_min value_max) [h1,h2,h3,h4],
                              profit_min < (sum $ map L.getPrice [h1,h2,h3,h4]),
                              profit_max > (sum $ map L.getPrice [h1,h2,h3,h4]),
                              or [
                                    time_max == 0, 
                                    time_max > (sum $ map (\(lv, tier) -> L.getTimeToLevel (1,tier) expchart global_tick_exp global_time_bw_ticks lv) [h1,h2,h3,h4]) 
                                  ],
                              angle <= L.getAngle (
                                  sum $ map (\(lv, tier) -> L.getTimeToLevel (1,tier) expchart global_tick_exp global_time_bw_ticks lv) [h1,h2,h3,h4], 
                                  (-) (sum $ map getPrice [h1,h2,h3,h4]) (sum $ map (\(_,tier) -> L.priceAtLv0 tier) [h1,h2,h3,h4])
                              ),
                              0.0 < (sum $ map (\(lv, tier) -> L.getTimeToLevel (1,tier) expchart global_tick_exp global_time_bw_ticks lv) [h1,h2,h3,h4])
                             
                            ]
                                             
    -- let all_prices = map (\(_,price,_) -> price) all_permutations
    -- let all_times  = map (\(_,_,time) -> time) all_permutations
    
    -- let expchart' = read (args !! 0) :: ExpChart
    
    -- putStrLn $ show time_t1lv1_t1lv3
    putStr $ encode "Running with the following parameters: \n"
    putStr $ encode "profit_min "
    putStr $ encode $ (show profit_min) ++ "\n"
    putStr $ encode "profit_max "
    putStr $ encode $ show profit_max ++ "\n"
    
    putStr $ encode "global_tick_exp "
    putStr $ encode $ (show global_tick_exp) ++ "\n"
    putStr $ encode "global_time_bw_ticks "
    putStr $ encode $ (show $ global_time_bw_ticks) ++ "\n"
    putStr $ encode "time_max " 
    putStr $ encode $ (show $ time_max) ++ "\n"
    
    putStr $ encode "value_min "
    putStr $ encode $ (show value_min) ++ "\n"
    putStr $ encode "value_max "
    putStr $ encode $ (show value_max) ++ "\n"
    
    putStr $ encode "level_min "
    putStr $ encode $ (show level_min) ++ "\n"
    putStr $ encode "level_max "
    putStr $ encode $ (show level_max) ++ "\n"
    
    putStr $ encode "tier_min "
    putStr $ encode $ (show tier_min) ++ "\n"
    putStr $ encode "tier_max "
    putStr $ encode $ (show tier_max) ++ "\n"
    
    putStr $ encode "angle "
    putStr $ encode $ (show angle) ++ "\n"

    putStr $ encode "Comparison on price to time ratio.\n"
    
    if length horsedata == 0 then
        do
            putStr $ encode "\n"
            putStr $ encode "No horses match the criteria!\n"
    else
        do
    
            let slice_num = maximum [slice,1 + ((length horsedata)^4 `div` slice)]
            let parts = filter (\x -> length x > 0) $ map (\n -> fst $ T.slice n slice all_permutations) [0,slice..slice_num]
            let ignore_assumed_best = (\(h,_,_) -> (length h) == 0) assumed_best
            
            let formal_head = replicate 4 $ head horsedata
            let formal_base = (
                                formal_head, 
                                sum $ map getPrice formal_head, 
                                sum $ map (\(lv, tier) -> L.getTimeToLevel (1,tier) expchart global_tick_exp global_time_bw_ticks lv) formal_head
                              )
            let formal_best = if ignore_assumed_best then formal_base else assumed_best
            
            putStr $ encode "Assumed best: "
            putStr $ encode $ show $ if ignore_assumed_best then (formal_best :: ([Horse],Exp,Time)) else (assumed_best :: ([Horse],Exp,Time))
            putStr $ encode $ (if ignore_assumed_best then " (ignored) " else "") ++ "\n"
            
            let result = if ignore_assumed_best 
                        then 
                            L.doEpicCalculation' params expchart $ map (\perms -> L.doEpicCalculation' params expchart perms) parts
                        else
                            L.doEpicCalculation params expchart formal_best $ map (\perms -> L.doEpicCalculation params expchart formal_best perms) parts
            
            putStr $ encode "\n"
            putStr $ encode "Result: \n"
            
            if (\(h,_,_) -> length h == 0) result then
                do putStr $ encode "No permutation matches the criteria!\n"
            else
                do 
                    putStr $ encode $ (show result) ++ "\n"
                    
                    
                    let lowest_tier_horse = (\(h,_,_) -> minimumBy (\(_,t1) (_,t2) -> compare t1 t2) h) result
                    let highest_tier_horse = (\(h,_,_) -> maximumBy (\(_,t1) (_,t2) -> compare t1 t2) h) result
                    let lowest_level_horse = (\(h,_,_) -> minimumBy (\(l1,_) (l2,_) -> compare l1 l2) h) result
                    let highest_level_horse = (\(h,_,_) -> maximumBy (\(l1,_) (l2,_) -> compare l1 l2) h) result
                    let tiers = sort $ nub $ map (\(_,t) -> t) $ (\(h,_,_) -> h) result
                    
                    let breed_options = [ (t1,t2) | t1 <- tiers, t2 <- tiers ];
                    
                    let breed_results = map (\pair -> B.renderBreedData pair (fst lowest_level_horse, fst highest_level_horse)) breed_options
                    
                    putStr $ encode "Using (minlevel,maxlevel) as "
                    putStr $ encode $ (show (fst lowest_level_horse, fst highest_level_horse)) ++ "\n"
                    putStr $ encode "The minimal breed combinations that guarantee the least chance to drop tiers are as follows:\n"
                    mapM_ (\line -> do l <- line ; putStr $ encode $ (show l) ++ "\n") breed_results 
                    
    return $ encode "- - - - - - - - - - -"