module Lib
    ( someFunc
    ) where

newtype Signal = Signal (String, Int)

newtype Rule a = MakeRule {
    
    calc:: ([Signal] -> Maybe Int) 
}




findSignal:: [Signal] -> String -> Maybe Int
findSignal ws sig = case ws of

    (s1:sx) -> if sig == name then Just value else findSignal sx sig
               where
                    Signal (name, value) = s1
    [s1] -> if sig == name then Just value else Nothing
            where
                Signal (name, value) = s1
    _ -> Nothing

-- return signal value or Just 0 if no signal present
testSignal:: [Signal] -> String -> Maybe Int
testSignal ws sig = case ws of

    (s1:sx) -> if sig == name then Just value else findSignal sx sig
                where
                    Signal (name, value) = s1
    [s1] -> if sig == name then Just value else Just 0
            where
                Signal (name, value) = s1
    _ -> Just 0
    


-- not operator
fNot::Rule [Signal] -> Rule [Signal]
fNot rule = MakeRule f
            where
                f sw = case calc rule sw of
                    Just v -> if v == 0 then Just 1 else Just 0
                    Nothing -> Nothing

-- and operator
(+++)::Rule [Signal] -> Rule [Signal] -> Rule [Signal]
rule1 +++ rule2 = MakeRule f
            where
                f sw = case calc rule1 sw of
                    Just v -> if v == 0 
                              then Just 0 
                              else calc rule2 sw

                    Nothing -> Nothing


-- or operator
(<|>)::Rule [Signal] -> Rule [Signal] -> Rule [Signal]
rule1 <|> rule2 = MakeRule f
            where
                f sw = case calc rule1 sw of
                    Just v -> if v /= 0 
                              then Just 1 
                              else calc rule2 sw

                    Nothing -> Nothing



-- FOGLIO 14 --

rBat::Rule [Signal]
rBat = MakeRule f
        where 
            f s = findSignal s "LS5" 


rVa::Rule [Signal]
rVa = MakeRule f
        where 
            f s = findSignal s "LS4" 
            
rVc::Rule [Signal]
rVc = MakeRule f
        where 
            f s = findSignal s "LS2"

rPre::Rule [Signal]
rPre = MakeRule f
        where 
            f s = findSignal s "B1"    

lS3::Rule [Signal]
lS3 = MakeRule f
        where 
            f s = findSignal s "LS3" 

rFa::Rule [Signal]
rFa = lS3 <|> (fNot rC2 +++ rFa1)     

rFa1::Rule [Signal]
rFa1 = rFa     



-- FOGLIO 13 --       

rL1::Rule [Signal]
rL1 = MakeRule f
        where 
            f s = findSignal s "RL1"            
            
rL2::Rule [Signal]
rL2 = MakeRule f
        where 
            f s = findSignal s "RL2"   

rL3::Rule [Signal]
rL3 = MakeRule f
        where 
            f s = findSignal s "RL3"   
           
r1 = fNot rM 
     +++ rBat 
     +++ fNot rMb
     +++ rVa
     +++ fNot rPre


rM::Rule [Signal]
rM = fNot rIteRv

-- FOGLIO 12 --      


part0::Rule [Signal]  
part0 = s2 +++ ((rItes +++ (s6 <|> rVc)) <|> rPre) +++ fNot rL1 +++ rL3

rItes::Rule [Signal]  -- czasowy, minuty - TEMPO BATTITORE / tempo uderzeń
rItes = btrItes <|> (fNot rM +++ part0) -- depends also from button (btrItes)

btrItes::Rule [Signal]
btrItes = MakeRule f
        where 
            f s = findSignal s "BtRITES"  


r2::Rule [Signal]
r2 = fNot rM +++ (fNot rItes <|> (r2' +++ rL2)) +++ part0

r2'::Rule [Signal]
r2' = MakeRule f
        where 
            f s = testSignal s "R2"  -- if R2 exists 


rIteRv::Rule [Signal]  -- czasowy
rIteRv = btrIteRv <|> (fNot r2 +++ rItes +++ part0)

btrIteRv::Rule [Signal]
btrIteRv = MakeRule f
        where 
            f s = findSignal s "BtRITE-RV"  


rMb::Rule [Signal]
rMb = rM +++ rFa +++ s2

sRb::Rule [Signal]
sRb = rItbat +++ fNot rBat +++ rFa +++ s2

sRm::Rule [Signal]
sRm = sRb

rItbat::Rule [Signal] -- sekundy - TEMPO BATTITORE / tempo uderzeń
rItbat = btrItbat <|> (rFa +++ s2)

btrItbat::Rule [Signal]
btrItbat = MakeRule f
        where 
            f s = findSignal s "BtRITBAT"  


-- FOGLIO 10--    


part1::Rule [Signal]  
part1 = (r2 +++ s3 +++ rPre) <|> (s3 +++ r2)


t1f::Rule [Signal]
t1f = t3f +++ fNot t2f +++ (t3f <|> rItp) +++ part1


t2f::Rule [Signal]
t2f = fNot t3f +++ fNot t1f +++ fNot rItp +++ part1

t3f::Rule [Signal]
t3f = fNot t2f +++ t3f' +++ part1


t3f'::Rule [Signal]
t3f' = MakeRule f
            where 
                f s = testSignal s "T3F"  -- if T3F exists 
    
rItp::Rule [Signal]
rItp = fNot t1f +++ part1
                

-- FOGLIO 8--    


rC2::Rule [Signal]
rC2 = fNot rPre +++ ((s5 +++ s7 +++ fNot p2) <|> 
                     (s5 +++ (r1 <|> t1f <|> t2f))
                    )









s2::Rule [Signal]
s2 = MakeRule f
        where 
            f s = findSignal s "S2"  
            
s3::Rule [Signal]
s3 = MakeRule f
        where 
            f s = findSignal s "S3"   

s5::Rule [Signal]
s5 = MakeRule f
        where 
            f s = findSignal s "S5"  

s6::Rule [Signal]
s6 = MakeRule f
        where 
            f s = findSignal s "S6"  

s7::Rule [Signal]
s7 = MakeRule f
        where 
            f s = findSignal s "S7"       


p2::Rule [Signal]
p2 = MakeRule f
        where 
            f s = findSignal s "P2"   
            
            
worldState = [

        Signal ("LS2", 0), 
        Signal ("LS3", 1), -- maksymalne rozciągnięcie (górna krańcówka)
        Signal ("LS4", 1), -- pozycja wału (dolna krańcówka)
        Signal ("LS5", 0), 
        Signal ("B1", 0),
        Signal ("S2", 1),  -- ?
        Signal ("S3", 1),  -- ?
        Signal ("S5", 1),  -- ?
        Signal ("S6", 1),  -- ?
        Signal ("S7", 1),  -- ?        
        Signal ("P2", 1),  -- ?    
        Signal ("RL1", 0), -- presenza fango (obecność błota)
        Signal ("RL2", 0), -- sonda fine filtrata (koniec filtrowania/brak wody)
        Signal ("RL3", 0), -- sonda raffreddamento tenute (utrzymanie schładzania)
        Signal ("BtRITES", 0), -- przycisk RITES
        Signal ("BtRITBAT", 0), -- przycisk RITES
        Signal ("BtRITE-RV", 0) -- przycisk BtRITE-RV
        --Signal ("R2", 0)    -- przekaźnik R2
        --Signal ("T3F", 0)    -- przekaźnik R2
        ]

someFunc :: IO ()
someFunc = do 
            let result = calc rItbat worldState
            putStrLn $ "Result: " ++ (show result)
