{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CPSLIO2 where

import Data.Maybe 

import Control.Monad.Reader
import Control.Monad.Cont
import Control.Monad.Trans

class Label l where
  lub :: l -> l -> l

newtype IFC s r a = IFC { unIFC :: ContT r (Reader s) a }
                  deriving (Functor, Monad, MonadCont, MonadReader s)

runIFC m e k = runReader (runContT (unIFC m) k) e

data TP = L | H
        deriving (Eq, Ord, Show)

type Name = Char
type Env l = [(Name, (Int, l))] 

instance Label TP where
  lub L L = L
  lub _ _ = H

{-
data CPS r a = MkCC { contt :: PC -> (PC -> a -> r) -> r }

instance Monad (CPS r) where 
  return x = MkCC $ \pc -> \k -> k pc x   
  m >>= f  = MkCC $ \pc -> \k -> contt m pc (\pc' -> \a -> contt (f a) pc' k) 

currentPC :: CPS r PC -- PC -> (PC -> a -> PC) -> PC 
currentPC = MkCC $ \pc -> \k -> k pc pc 
                                
bracketPC :: PC -> CPS r a -> CPS r a 
bracketPC l (MkCC c) = MkCC $ \pc -> \k -> c (join pc l) (\pc' -> \a -> k pc a) -- Ignores the pc comming after c finishes!
-}

type LIOState l = (l, Env l)

getLabel :: Label l => IFC (LIOState l) r l
getLabel = fmap fst ask

putLabel :: Label l => l -> IFC (LIOState l) r ()
putLabel l = callCC $ \k -> local (\(_,e) -> (l,e)) (k ())

setEnv :: Label l => Name -> Int -> IFC (LIOState l) r ()
setEnv x v = callCC $ \k -> local (\(l,e) -> (l, addTo e x (v,l))) (k ())
  where addTo e x p = (x,p) : case lookup x e of
          Nothing -> e
          Just _  -> [ (a,b) | (a,b) <- e, a /= x ]

data Labeled l a = MkLabel l a
     deriving (Eq, Ord, Show)

data LIO l a where
     Return  :: a -> LIO l a
     Bind    :: LIO l a -> (a -> LIO l b) -> LIO l b 
     Unlabel :: Labeled l a -> LIO l a 
     Assign  :: Name -> Int -> LIO l ()


lioCPS :: Label l => LIO l a -> IFC (LIOState l) r a 
lioCPS (Return x) = return x
lioCPS (Bind m f) = lioCPS m >>= lioCPS . f
lioCPS (Unlabel (MkLabel l x)) = do pc <- getLabel 
                                    putLabel (lub l pc)
                                    return x
lioCPS (Assign x v) = setEnv x v

env1 :: [(Name, (Int, TP))]
env1 = [('h',(5,H)), ('l',(5,L))]  

-- This example shows 3
exLIO = Bind (Return 1) (\x -> Bind (Unlabel (MkLabel H 2)) (\y -> Return (x+y))) 

-- This one should show an error!
exLIO2 = Bind (Return 1) (\x -> Bind (Unlabel (MkLabel H 2)) (\y -> Bind (Assign 'l' 1) (\_ -> Return (x+y)))) 

run_example2 :: LIO TP a -> a 
run_example2 ex = runIFC (lioCPS ex) (L,env1) return

-- No calling k, not failing!
try :: IFC (LIOState TP) r Integer 
try = do callCC (\k -> do k 1
                          lioCPS exLIO)
         lioCPS (Assign 'l' 1)
         return 0
      
-- No calling k, but falling since the current label is high
try2 :: IFC (LIOState TP) r Integer 
try2 = do callCC (\k -> lioCPS exLIO)
          lioCPS (Assign 'l' 1)
          return 0
       

           
-- reader :: pc -> ((a -> r) -> r)
-- reader PC (CPS r a)

-- LIO ~
-- CPST (Reader PC) r a ~ (a -> Reader PC r) -> Reader PC r
--                      ~ (a -> PC -> r) -> PC ->  r

-- get, put
-- newIOREf, readIORef, writeIORef

-- Cont (Reader s) r a ~?~ Cont (State s) r a  