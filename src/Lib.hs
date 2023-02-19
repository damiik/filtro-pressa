module Lib ( someFunc ) where

-- monad version ---
-- signal: Name, Value
newtype Signal = Signal (String, Int)

-- signal world: signals list, log string
newtype SignalW = SignalW ([Signal], String)

newtype Rule a = MakeRule {
    
    calc:: SignalW -> (Maybe Int, String)
}




findSignal:: SignalW -> String -> (Maybe Int, String)
findSignal ws sig = case ws of

    SignalW (s1:sx, log') -> if sig == name 
                              then (Just value, log'++" found: " ++ sig ++ "!")
                              else findSignal (SignalW (sx, log')) sig
                              where
                                 Signal (name, value) = s1
    SignalW ([s1], log') ->  if sig == name 
                            then (Just value, log'++" found: " ++ sig ++ "!")
                            else (Nothing,  log'++" cant find: " ++ sig ++ "?")
                            where
                                Signal (name, value) = s1
    SignalW (_, log') -> (Nothing, log'++"cant find: " ++ sig ++ "?")

-- return signal value or Just 0 if no signal present
testSignal:: SignalW -> String -> (Maybe Int, String)
testSignal ws sig = case ws of

    SignalW (s1:sx, log') ->   if sig == name 
                                then (Just value, log'++" tested: " ++ sig ++ "!")
                                else testSignal (SignalW (sx, log')) sig
                                where
                                    Signal (name, value) = s1
    SignalW ([s1], log') ->  if sig == name 
                            then (Just value, log'++" tested: " ++ sig ++ "!")
                            else (Just 0, log'++" cant test: " ++ sig ++ "?")
                            where
                                Signal (name, value) = s1
    SignalW (_, log') -> (Just 0, log'++" cant test: " ++ sig ++ "?")
    


-- log operator
(>>>)::String -> Rule SignalW -> Rule SignalW
s >>> rule = MakeRule f
             where
                f (SignalW (sw, log)) = case calc rule (SignalW (sw, log ++ s++ " ")) of
                                        (result, log) ->  (result, log)





-- not operator
fNot::Rule SignalW -> Rule SignalW
fNot rule = MakeRule f
            where
                f sw = case calc rule sw of
                    (Just v, log) -> if v == 0 
                                        then (Just 1, log++" not") 
                                        else (Just 0, log++" not")
                    (Nothing, log) -> (Nothing, log++" not")

-- and operator
(+++)::Rule SignalW -> Rule SignalW -> Rule SignalW
rule1 +++ rule2 = MakeRule f
            where
                f sw = case calc rule1 sw of
                    (Just v, log) -> if v == 0 
                                        then (Just 0, log++" +++")
                                        else                                  
                                            calc rule2 $ SignalW (signals, log++" +++")
                                            where 
                                                SignalW (signals, log') = sw

                    (Nothing, log) -> (Nothing, log++" +++")


-- or operator
(<|>)::Rule SignalW -> Rule SignalW -> Rule SignalW
rule1 <|> rule2 = MakeRule f
            where
                f sw = case calc rule1 sw of
                    (Just v, log) -> if v /= 0 
                                        then (Just 1, log++" <|>")
                                        else 
                                            calc rule2 $ SignalW (signals, log'++" <|>")
                                            where 
                                                SignalW (signals, log') = sw

                    (Nothing, log) -> (Nothing, log++" <|>")



-- FOGLIO 14 --

rBat::Rule SignalW
rBat = "rBat" >>> MakeRule f
                    where 
                        f s = findSignal s "LS5" 


rVa::Rule SignalW
rVa = "rVa" >>> MakeRule f
        where 
            f s = findSignal s "LS4" 
            
rVc::Rule SignalW
rVc = "rVc" >>> MakeRule f
        where 
            f s = findSignal s "LS2"

rPre::Rule SignalW
rPre =  "rPre" >>> MakeRule f
        where 
            f s = findSignal s "B1"    

lS3::Rule SignalW
lS3 = "LS3" >>> MakeRule f
        where 
            f s = findSignal s "LS3" 

rFa::Rule SignalW
rFa = "rFa" >>> lS3 <|> (fNot rC2 +++ rFa1')     

rFa1::Rule SignalW
rFa1 = "rFa1" >>>  rFa     

rFa1'::Rule SignalW
rFa1' = "rFa1'" >>> MakeRule f
        where 
            f s = testSignal s "RFA1"  -- if R2 exists 

-- FOGLIO 13 --       

rL1::Rule SignalW
rL1 = "rL1" >>> MakeRule f
        where 
            f s = findSignal s "RL1"            
            
rL2::Rule SignalW
rL2 = "rL2" >>> MakeRule f
        where 
            f s = findSignal s "RL2"   
           
rL3::Rule SignalW
rL3 = "rL3" >>> MakeRule f
        where 
            f s = findSignal s "RL3"   
           
r1 =  "r1" >>> fNot rM 
     +++ rBat 
     +++ fNot rMb
     +++ rVa
     +++ fNot rPre


rM::Rule SignalW
rM =  "rM" >>> fNot rIteRv

-- FOGLIO 12 --      


part0::Rule SignalW  
part0 =  "part0" >>> s2 +++ ((rItes +++ (s6 <|> rVc)) <|> rPre) +++ fNot rL1 +++ rL3

rItes::Rule SignalW  -- czasowy, minuty - TEMPO BATTITORE / tempo uderzeń
rItes = "rItes" >>> btrItes <|> (fNot rM +++ part0) -- depends also from button (btrItes)

btrItes::Rule SignalW
btrItes = "btrItes" >>> MakeRule f
        where 
            f s = findSignal s "BtRITES"  


r2::Rule SignalW
r2 = "r2" >>> fNot rM +++ (fNot rItes <|> (r2' +++ rL2)) +++ part0

r2'::Rule SignalW
r2' = "r2'" >>> MakeRule f
        where 
            f s = testSignal s "R2"  -- if R2 exists 


rIteRv::Rule SignalW  -- czasowy
rIteRv = "rIteRv" >>> btrIteRv <|> (fNot r2 +++ rItes +++ part0)

btrIteRv::Rule SignalW
btrIteRv = "btrIteRv" >>> MakeRule f
        where 
            f s = findSignal s "BtRITE-RV"  


rMb::Rule SignalW
rMb = "rMb" >>> rM +++ rFa +++ s2

sRb::Rule SignalW
sRb = "sRb" >>> rItbat +++ fNot rBat +++ rFa +++ s2

sRm::Rule SignalW
sRm = "sRm" >>> sRb

rItbat::Rule SignalW -- sekundy - TEMPO BATTITORE / tempo uderzeń
rItbat =  "rItbat" >>> btrItbat <|> (rFa +++ s2)

btrItbat::Rule SignalW
btrItbat = "btrItbat" >>> MakeRule f
        where 
            f s = findSignal s "BtRITBAT"  


-- FOGLIO 10--    


part1::Rule SignalW
part1 = "part1" >>> (r2 +++ s3 +++ rPre) <|> (s3 +++ r2)

-- veloce pompa gindori
t1f::Rule SignalW
t1f = "t1f" >>> t3f +++ fNot t2f +++ (t3f <|> rItp) +++ part1


-- lenta pompa gindori
t2f::Rule SignalW
t2f = "t2f" >>> fNot t3f +++ fNot t1f +++ fNot rItp +++ part1

-- <star> pompa gindori
t3f::Rule SignalW
t3f = "t3f" >>> fNot t2f +++ t3f' +++ part1


t3f'::Rule SignalW
t3f' = "t3f'" >>> MakeRule f
            where 
                f s = testSignal s "T3F"  -- if T3F exists 
    
rItp::Rule SignalW
rItp = "rItp" >>> fNot t1f +++ part1
                

-- FOGLIO 8--    


rC2::Rule SignalW
rC2 = "rC2" >>> fNot rPre +++ ((s5 +++ s7 +++ fNot p2) <|> 
                     (s5 +++ (r1 <|> t1f <|> t2f))
                    )









s2::Rule SignalW
s2 = "s2" >>> MakeRule f
        where 
            f s = findSignal s "S2"  
            
s3::Rule SignalW
s3 = "s3" >>> MakeRule f
        where 
            f s = findSignal s "S3"   

s5::Rule SignalW
s5 = "s5" >>> MakeRule f
        where 
            f s = findSignal s "S5"  

s6::Rule SignalW
s6 = "s6" >>> MakeRule f
        where 
            f s = findSignal s "S6"  

s7::Rule SignalW
s7 = "s7" >>> MakeRule f
        where 
            f s = findSignal s "S7"       


p2::Rule SignalW
p2 = "p2" >>> MakeRule f
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
        Signal ("BtRITBAT", 0), -- przycisk RITBAT
        Signal ("BtRITE-RV", 0) -- przycisk BtRITE-RV
        --Signal ("R2", 0)    -- przekaźnik R2
        --Signal ("T3F", 0)    -- przekaźnik T3F
        --Signal ("RFA1", 0)    -- przekaźnik RFA1
        ]

someFunc :: IO ()
someFunc = do 
            let result = calc rItbat $ SignalW (worldState, "log:")
            putStrLn $ "Result: " ++ show result
