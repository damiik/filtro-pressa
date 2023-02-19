module Lib ( someFunc ) where

-- monad version ---
-- signal: Name, Value
newtype Signal = Signal (String, Int)

-- signal world: signals list, log string
newtype SignalW = SignalW ([Signal], String)

newtype Rule a = MakeRule {
    
    calc:: SignalW -> (a, String)
}

newtype SignalValue a = SignalValue (Maybe a)


-- instance Functor Rule where
--   fmap f r = MakeRule (f $ calc r)




-- instance Applicative Identity where
--   pure = Identity
--   (Identity f) <*> (Identity a) = Identity (f a)


findSignal:: SignalW -> String -> (SignalValue Int, String)
findSignal ws sig = case ws of

    SignalW (s1:sx, log') -> if sig == name 
                              then (SignalValue (Just value), log'++" found: " ++ sig ++ "!")
                              else findSignal (SignalW (sx, log')) sig
                              where
                                 Signal (name, value) = s1
    SignalW ([s1], log') ->  if sig == name 
                            then (SignalValue (Just value), log'++" found: " ++ sig ++ "!")
                            else (SignalValue Nothing,  log'++" cant find: " ++ sig ++ "?")
                            where
                                Signal (name, value) = s1
    SignalW (_, log') -> (SignalValue Nothing, log'++"cant find: " ++ sig ++ "?")

-- return signal value or Just 0 if no signal present
testSignal:: SignalW -> String -> (SignalValue Int, String)
testSignal ws sig = case ws of

    SignalW (s1:sx, log') ->   if sig == name 
                                then (SignalValue (Just value), log'++" tested: " ++ sig ++ "!")
                                else testSignal (SignalW (sx, log')) sig
                                where
                                    Signal (name, value) = s1
    SignalW ([s1], log') ->  if sig == name 
                            then (SignalValue (Just value), log'++" tested: " ++ sig ++ "!")
                            else (SignalValue (Just 0), log'++" cant test: " ++ sig ++ "?")
                            where
                                Signal (name, value) = s1
    SignalW (_, log') -> (SignalValue (Just 0), log'++" cant test: " ++ sig ++ "?")
    


-- log operator
(>>>)::String -> Rule (SignalValue Int) -> Rule (SignalValue Int)
s >>> rule = MakeRule f
             where
                f (SignalW (sw, log)) = case calc rule (SignalW (sw, log ++ s++ " ")) of
                                        (result, log) ->  (result, log)





-- not operator
fNot::Rule (SignalValue Int)  -> Rule (SignalValue Int) 
fNot rule = MakeRule f
            where
                f sw = case calc rule sw of
                    (SignalValue (Just v), log) -> if v == 0 
                                        then (SignalValue $ Just 1, log++" not") 
                                        else (SignalValue $ Just 0, log++" not")
                    (SignalValue Nothing, log) -> (SignalValue Nothing, log++" not")

-- and operator
(+++)::Rule (SignalValue Int) -> Rule (SignalValue Int) -> Rule (SignalValue Int)
rule1 +++ rule2 = MakeRule f
            where
                f sw = case calc rule1 sw of
                    (SignalValue (Just v), log) -> if v == 0 
                                        then (SignalValue (Just 0), log++" +++")
                                        else                                  
                                            calc rule2 $ SignalW (signals, log++" +++")
                                            where 
                                                SignalW (signals, log') = sw

                    (SignalValue Nothing, log) -> (SignalValue Nothing, log++" +++")


-- or operator
(<|>)::Rule (SignalValue Int) -> Rule (SignalValue Int) -> Rule (SignalValue Int)
rule1 <|> rule2 = MakeRule f
            where
                f sw = case calc rule1 sw of
                    (SignalValue (Just v), log) -> if v /= 0 
                                        then (SignalValue (Just 1), log++" <|>")
                                        else 
                                            calc rule2 $ SignalW (signals, log'++" <|>")
                                            where 
                                                SignalW (signals, log') = sw

                    (SignalValue Nothing, log) -> (SignalValue Nothing, log++" <|>")



-- FOGLIO 14 --

rBat::Rule (SignalValue Int)
rBat = "rBat" >>> MakeRule f
                    where 
                        f s = findSignal s "LS5" 


rVa::Rule (SignalValue Int)
rVa = "rVa" >>> MakeRule f
        where 
            f s = findSignal s "LS4" 
            
rVc::Rule (SignalValue Int)
rVc = "rVc" >>> MakeRule f
        where 
            f s = findSignal s "LS2"

rPre::Rule (SignalValue Int)
rPre =  "rPre" >>> MakeRule f
        where 
            f s = findSignal s "B1"    

lS3::Rule (SignalValue Int)
lS3 = "LS3" >>> MakeRule f
        where 
            f s = findSignal s "LS3" 

rFa::Rule (SignalValue Int)
rFa = "rFa" >>> lS3 <|> (fNot rC2 +++ rFa1')     

rFa1::Rule (SignalValue Int)
rFa1 = "rFa1" >>>  rFa     

rFa1'::Rule (SignalValue Int)
rFa1' = "rFa1'" >>> MakeRule f
        where 
            f s = testSignal s "RFA1"  -- if R2 exists 

-- FOGLIO 13 --       

rL1::Rule (SignalValue Int)
rL1 = "rL1" >>> MakeRule f
        where 
            f s = findSignal s "RL1"            
            
rL2::Rule (SignalValue Int)
rL2 = "rL2" >>> MakeRule f
        where 
            f s = findSignal s "RL2"   
           
rL3::Rule (SignalValue Int)
rL3 = "rL3" >>> MakeRule f
        where 
            f s = findSignal s "RL3"   
           
r1 =  "r1" >>> fNot rM 
     +++ rBat 
     +++ fNot rMb
     +++ rVa
     +++ fNot rPre


rM::Rule (SignalValue Int)
rM =  "rM" >>> fNot rIteRv

-- FOGLIO 12 --      


part0::Rule (SignalValue Int)  
part0 =  "part0" >>> s2 +++ ((rItes +++ (s6 <|> rVc)) <|> rPre) +++ fNot rL1 +++ rL3

rItes::Rule (SignalValue Int)  -- czasowy, minuty - TEMPO BATTITORE / tempo uderzeń
rItes = "rItes" >>> btrItes <|> (fNot rM +++ part0) -- depends also from button (btrItes)

btrItes::Rule (SignalValue Int)
btrItes = "btrItes" >>> MakeRule f
        where 
            f s = findSignal s "BtRITES"  


r2::Rule (SignalValue Int)
r2 = "r2" >>> fNot rM +++ (fNot rItes <|> (r2' +++ rL2)) +++ part0

r2'::Rule (SignalValue Int)
r2' = "r2'" >>> MakeRule f
        where 
            f s = testSignal s "R2"  -- if R2 exists 


rIteRv::Rule (SignalValue Int)  -- czasowy
rIteRv = "rIteRv" >>> btrIteRv <|> (fNot r2 +++ rItes +++ part0)

btrIteRv::Rule (SignalValue Int)
btrIteRv = "btrIteRv" >>> MakeRule f
        where 
            f s = findSignal s "BtRITE-RV"  


rMb::Rule (SignalValue Int)
rMb = "rMb" >>> rM +++ rFa +++ s2

sRb::Rule (SignalValue Int)
sRb = "sRb" >>> rItbat +++ fNot rBat +++ rFa +++ s2

sRm::Rule (SignalValue Int)
sRm = "sRm" >>> sRb

rItbat::Rule (SignalValue Int) -- sekundy - TEMPO BATTITORE / tempo uderzeń
rItbat =  "rItbat" >>> btrItbat <|> (rFa +++ s2)

btrItbat::Rule (SignalValue Int)
btrItbat = "btrItbat" >>> MakeRule f
        where 
            f s = findSignal s "BtRITBAT"  


-- FOGLIO 10--    


part1::Rule (SignalValue Int)
part1 = "part1" >>> (r2 +++ s3 +++ rPre) <|> (s3 +++ r2)

-- veloce pompa gindori
t1f::Rule (SignalValue Int)
t1f = "t1f" >>> t3f +++ fNot t2f +++ (t3f <|> rItp) +++ part1


-- lenta pompa gindori
t2f::Rule (SignalValue Int)
t2f = "t2f" >>> fNot t3f +++ fNot t1f +++ fNot rItp +++ part1

-- <star> pompa gindori
t3f::Rule (SignalValue Int)
t3f = "t3f" >>> fNot t2f +++ t3f' +++ part1


t3f'::Rule (SignalValue Int)
t3f' = "t3f'" >>> MakeRule f
            where 
                f s = testSignal s "T3F"  -- if T3F exists 
    
rItp::Rule (SignalValue Int)
rItp = "rItp" >>> fNot t1f +++ part1
                

-- FOGLIO 8--    


rC2::Rule (SignalValue Int)
rC2 = "rC2" >>> fNot rPre +++ ((s5 +++ s7 +++ fNot p2) <|> 
                     (s5 +++ (r1 <|> t1f <|> t2f))
                    )









s2::Rule (SignalValue Int)
s2 = "s2" >>> MakeRule f
        where 
            f s = findSignal s "S2"  
            
s3::Rule (SignalValue Int)
s3 = "s3" >>> MakeRule f
        where 
            f s = findSignal s "S3"   

s5::Rule (SignalValue Int)
s5 = "s5" >>> MakeRule f
        where 
            f s = findSignal s "S5"  

s6::Rule (SignalValue Int)
s6 = "s6" >>> MakeRule f
        where 
            f s = findSignal s "S6"  

s7::Rule (SignalValue Int)
s7 = "s7" >>> MakeRule f
        where 
            f s = findSignal s "S7"       


p2::Rule (SignalValue Int)
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
            let result = calc rItbat $ (Maybe Int) (worldState, "log:")
            putStrLn $ "Result: " ++ show result
