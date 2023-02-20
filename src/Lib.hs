{-# LANGUAGE TupleSections #-}
module Lib ( someFunc ) where

-- monad version ---
-- signal: Name, Value
newtype Signal = Signal (String, Int)
    deriving Show

-- signal world: signals list, log string
newtype SignalW = SignalW ([Signal], String)
    deriving Show

newtype Rule a = MakeRule {
    
    calc:: SignalW -> (a, SignalW)
}

-- newtype Rule a = MakeRule {
    
--     calc:: SignalW -> (a, SignalW)
-- }


newtype SignalValue a = SignalValue (Maybe a)
    deriving Show



instance Functor Rule where  -- fmap needed for Applicative   <$>
fmap :: (a -> b) -> Rule (SignalValue a) -> Rule (SignalValue b)
fmap f (MakeRule fp) = MakeRule $ 
    \st -> case fp st of
        (SignalValue(Just v), s) -> (SignalValue(Just $ f v), s)
        (_, s) -> (SignalValue Nothing, s)
                        
                              

instance Applicative Rule where
    -- create parser which return x and input
    pure x = MakeRule (x,)
    -- (<*>) :: Rule (a -> b) -> Rule a -> Rule b
    pf <*> px = MakeRule $ 
        \(SignalW (ws, s)) -> 
            case calc pf (SignalW (ws, s)) of
                (fa, s') -> case calc px s' of
                    (v, s'') -> (fa v, s'')

                                                    -- calc px (SignalW (ws, s))



instance Monad Rule where

    -- return x = MakeRule $ \s -> (x, s)

    -- (>>=) :: Rule a -> (a -> Rule b) -> Rule b    
    (MakeRule fp) >>= f = MakeRule $
        \st -> case fp st of
            (v, st') -> calc (f v) st'




findSignal:: SignalW -> String -> (SignalValue Int, SignalW)
findSignal ws sig = case ws of

    SignalW (s1:sx, log') -> 
        if sig == name 
        then (SignalValue (Just value), SignalW (s1:sx, log'++" found: " ++ sig ++ "!"))
        else findSignal (SignalW (sx, log')) sig
        where
            Signal (name, value) = s1

    SignalW ([], log') -> (SignalValue Nothing, SignalW ([], log'++"cant find: " ++ sig ++ "?"))



-- return signal value or Just 0 if no signal present
testSignal:: SignalW -> String -> (SignalValue Int, SignalW)
testSignal ws sig = case ws of

    SignalW (s1:sx, log') -> 
        if sig == name 
        then (SignalValue (Just value), SignalW (s1:sx, log'++" tested: " ++ sig ++ "!"))
        else testSignal (SignalW (sx, log')) sig
        where
            Signal (name, value) = s1

    SignalW ([], log') -> (SignalValue (Just 0),  SignalW ([], log'++" cant test: " ++ sig ++ "?"))
    


-- log operator
(>>>)::String -> Rule (SignalValue Int) -> Rule (SignalValue Int)
s >>> rule = MakeRule f
    where
<<<<<<< Updated upstream
    f (SignalW (sw, l)) = case calc rule (SignalW (sw,  l ++ s ++ " ")) of
=======
    f (SignalW (sw, l)) = case calc rule (SignalW (sw, l ++ s++ " ")) of
>>>>>>> Stashed changes
        (result, l') ->  (result, l')




-- not operator
fNot::Rule (SignalValue Int)  -> Rule (SignalValue Int) 
fNot rule = MakeRule f
    where
        f sw = case calc rule sw of
            (SignalValue (Just v), SignalW (st, log')) -> 
                if v == 0 
                then (SignalValue $ Just 1, SignalW (st, log'++" not")) 
                else (SignalValue $ Just 0, SignalW (st, log'++" not"))
            (SignalValue Nothing, SignalW (st, log')) -> (SignalValue Nothing, SignalW(st, log'++" not"))

-- and operator
(+++)::Rule (SignalValue Int) -> Rule (SignalValue Int) -> Rule (SignalValue Int)
-- rule1 +++ rule2 = MakeRule f

(MakeRule p1) +++ (MakeRule p2) = MakeRule $ 
    \input -> case p1 input of
        (SignalValue r1, rest) -> case p2 rest of
            (SignalValue r2, rest2) -> case (r1, r2) of
                (Just v1, Just v2) -> (SignalValue $ Just (if v1 > 0 && v2 > 0 then 1 else 0), rest2)
                _ -> (SignalValue Nothing, rest2)



-- or operator
(<|>)::Rule (SignalValue Int) -> Rule (SignalValue Int) -> Rule (SignalValue Int)
(MakeRule rule1f) <|> (MakeRule rule2f) = MakeRule $ 
    \input -> case rule1f input of
        (SignalValue (Just v1), rest) | v1 > 0 -> (SignalValue (Just v1), rest)
        _ -> rule2f input


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
            let result = calc rItbat $ SignalW (worldState, "log:")
            putStrLn $ "Result: " ++ show result
