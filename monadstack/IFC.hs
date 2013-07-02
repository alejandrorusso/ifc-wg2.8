{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
module IFC where

import Data.Maybe 

import Control.Monad.Reader
import Control.Monad.Cont
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Writer

class Label l where
  bottom :: l
  top :: l
  lub :: l -> l -> l
  lrt :: l -> l -> Bool

instance Label l => Monoid l where
  mempty = bottom
  mappend = lub  
                       
newtype RWT s m a = RWT { unRWT :: StateT s m a }
                  deriving (Functor, Monad, MonadIO)

instance Monad m => MonadReader s (RWT s m) where
  ask = RWT get
  local f m = RWT $ do st <- get
                       put (f st)
                       v <- unRWT m
                       put st
                       return v

instance (Monoid s, Monad m) => MonadWriter s (RWT s m) where
  tell x = RWT $ modify (mappend x)
  listen m = RWT $ do r <- unRWT m
                      s <- get
                      return (r, s)
  pass m = RWT $ do (a, f) <- unRWT m
                    modify f
                    return a

runRWT m s = runStateT (unRWT m) s

class (Label l) => WriteEffect l m where
  guardC :: l -> m ()
  
class (Label l) => ReadEffect l m where
  taint :: l -> m ()

newtype IFC l a = IFC { unIFC :: RWT l IO a }
                deriving (Functor, Monad, MonadIO, MonadReader l)
                         
runIFC m = runRWT (unIFC m) bottom

instance Label l => WriteEffect l (IFC l) where
  guardC l = IFC $ do pc <- ask
                      if (pc `lrt` l)
                        then return ()
                        else liftIO $ ioError (userError "security violation")

instance Label l => ReadEffect l (IFC l) where
  taint = IFC . tell

data TP = L | H
        deriving (Eq, Ord, Show)

type Name = Char
type Env l = [(Name, (Int, l))] 

instance Label TP where
  bottom = L
  top = H
  lub L L = L
  lub _ _ = H
  lrt H L = False
  lrt _ _ = True

getLabel :: Label l => IFC l l
getLabel = IFC $ ask

data Labeled l a = MkLabel l a
     deriving (Eq, Ord, Show)

data LIO l a where
     Return  :: a -> LIO l a
     Bind    :: LIO l a -> (a -> LIO l b) -> LIO l b 
     Unlabel :: Labeled l a -> LIO l a 
     Label   :: l -> a -> LIO l (Labeled l a)
--     Assign  :: Name -> Int -> LIO l ()

data Exp where 
     Con :: Int -> Exp 
     Add :: Exp -> Exp -> Exp 
     Var :: Name -> Exp 
     deriving Show

data Cmd where 
     Assgn :: Name -> Exp -> Cmd 
     Seq   :: Cmd -> Cmd -> Cmd 
     If    :: Exp -> Cmd -> Cmd -> Cmd 
     deriving Show

level :: Label l => Exp -> Env l -> l
level (Con _)     env = bottom
level (Var v)     env = snd $ fromJust $ lookup v env
level (Add e1 e2) env = level e1 env `lub` level e2 env

eval :: Label l => Exp -> Env l -> Int
eval (Con x) env     = x 
eval (Var x) env     = fst $ fromJust $ lookup x env
eval (Add e1 e2) env = eval e1 env + eval e2 env 

dsSem :: Label l => Cmd -> Env l -> IFC l (Env l)
dsSem (Assgn x e) env = local id $ do taint (level e env)
                                      let s = snd $ fromJust $ lookup x env
                                      guardC s
                                      return $ (x, (eval e env,s)) : env   
dsSem (Seq c1 c2) env = dsSem c1 env >>= dsSem c2
dsSem (If e c1 c2) env = local (lub (level e env)) $ 
          do if eval e env /= 0
                then dsSem c1 env
                else dsSem c2 env

lioSem :: Label l => LIO l a -> IFC l a 
lioSem (Return x) = return x
lioSem (Bind m f) = lioSem m >>= lioSem . f
lioSem (Unlabel (MkLabel l x)) = taint l >> return x
lioSem (Label l x) = guardC l >> return (MkLabel l x)
  
--lioCPS (Assign x v) = setEnv x v

env1 :: [(Name, (Int, TP))]
env1 = [('h',(5,H)), ('l',(5,L))]  

-- This example shows 3
exLIO = Bind (Return 1) (\x -> Bind (Unlabel (MkLabel H 2)) (\y -> Return (x+y))) 

runEx = runIFC $ lioSem exLIO

ex1 = If (Var 'h') (Assgn 'h' (Con 5))
                   (Assgn 'h' (Con 9))
  
ex2 = If (Var 'h') (Assgn 'l' (Con 5))
                   (Assgn 'l' (Con 9))
      
ex3 = Seq ex1 (Assgn 'l' (Con 1))
