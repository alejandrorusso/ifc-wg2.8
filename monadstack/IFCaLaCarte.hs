{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, TypeOperators, FlexibleContexts #-}
module IFC where

import Data.Maybe 

import AlaCarte
import Control.Monad.Free
import Control.Monad.Reader
import Control.Monad.Cont
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Writer
import Control.Arrow

class Label l where
  bottom :: l
  top :: l
  lub :: l -> l -> l
  lrt :: l -> l -> Bool

instance Label l => Monoid l where
  mempty = bottom
  mappend = lub  
                       
data Protect l a = Protect l a 

data ReadEffect l a where 
  Taint :: l -> a -> ReadEffect l a
  
  
data Local l a where
  Local :: l -> a -> Local l a
  

instance Functor (Local l) where
  fmap f (Local l a) = Local l (f a)

instance Functor (ReadEffect l) where
  fmap f (Taint l a) = Taint l (f a)

data WriteEffect l a where
  Guard :: l -> a -> WriteEffect l a
  
instance Functor (WriteEffect l) where
  fmap f (Guard l a) = Guard l (f a)

-- data STEffect s a where
--   Get :: (s -> a) -> STEffect s a
--   Put :: s -> a -> STEffect s a
  
inject :: (g :<: f) => g (Free f a) -> Free f a
inject = Impure . inj

taint :: (ReadEffect l :<: f) => l -> Free f ()
taint l = inject (Taint l (Pure ()))

guardC :: (WriteEffect l :<: f) => l -> Free f ()
guardC l = inject (Guard l (Pure ()))

localC :: (Local l :<: f) => l -> Free f a -> Free f a 
localC l a = inject (Local l a)

-- getC :: (STEffect s :<: f) => Free f s
-- getC = inject (Get Pure)

-- putC :: (STEffect s :<: f) => s -> Free f ()
-- putC s = inject (Put s (Pure ()))


-- localC :: forall l a . Label l => IFC l a -> IFC l a
-- localC m =
--   do (s :: l) <- getC
--      x <- m
--      putC s
--      return x


class (Functor f) => Run pc f where
  runAlg :: f (pc -> Maybe (a, pc)) -> pc -> Maybe (a,pc)

instance Label l => Run l (ReadEffect l) where
  runAlg (Taint l f) pc = f (pc `lub` l)

instance Label l => Run l (WriteEffect l) where
  runAlg (Guard l f) pc | pc `lrt` l = f pc
                        | otherwise  = Nothing

-- instance Label l => Run l (STEffect l) where
--   runAlg (Get f)   pc = f pc pc
--   runAlg (Put s f) pc = f s
              
instance (Run pc f, Run pc g) => Run pc (f :+: g) where
  runAlg (Inl x) = runAlg x
  runAlg (Inr x) = runAlg x
  
-- Quite clear that we forget about the PC! However, it does not work with the examples.
instance  Label l => Run l (Local l) where
  runAlg (Local l g) pc = do (a,_) <- g pc  
                             return (a,pc)
                             
runIFC :: Run l f => Free f a -> l -> Maybe (a, l)
runIFC = foldFree (\x pc -> Just (x,pc)) runAlg

type IFC l a = Free (WriteEffect l :+: ReadEffect l :+: Local l) a

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

data Labeled l a = MkLabel l a
     deriving (Eq, Ord, Show)

data LIO l a where
     Return  :: a -> LIO l a
     Bind    :: LIO l a -> (a -> LIO l b) -> LIO l b 
     Unlabel :: Labeled l a -> LIO l a 
     Label   :: l -> a -> LIO l (Labeled l a)
     ToLbl   :: l -> LIO l a -> LIO l (Labeled l a)

instance Monad (LIO l) where
  return = Return
  (>>=) = Bind

{-
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
-}

lioSem :: forall l a. Label l => LIO l a -> IFC l a 
lioSem (Return x) = return x
lioSem (Bind m f) = lioSem m >>= lioSem . f
lioSem (Unlabel (MkLabel l x)) = taint l >> return x
lioSem (Label l x) = guardC l >> return (MkLabel l x)
lioSem (ToLbl l m) = do x <- localC l (lioSem m)
                        lioSem (Label l x)
                        
--lioSem (ToLbl l m) = localC $ do x <- lioSem m
--                                  lioSem (Label l x)         

--lioCPS (Assign x v) = setEnv x v

env1 :: [(Name, (Int, TP))]
env1 = [('h',(5,H)), ('l',(5,L))]  

-- This example shows 3
exLIO = do x <- return 1
           y <- Unlabel (MkLabel H 2)
           return (x+y)

exLIO2 = do x <- return 1
            y <- ToLbl H $ do v <- Unlabel (MkLabel H 2)
                              return (v+x)
            z <- Unlabel y
            return z

exTolbl1 = do ToLbl H $ Unlabel (MkLabel H 2)
              Label L 1
              
exTolbl2 = do n <- ToLbl L $ Unlabel (MkLabel L 2)
              return n
              
exTolbl3 = do ToLbl H $ Unlabel (MkLabel H 2)
              Label L 1
              
runEx :: LIO TP a -> Maybe (a, TP)
runEx m = runIFC (lioSem m) L

{-
ex1 = If (Var 'h') (Assgn 'h' (Con 5))
                   (Assgn 'h' (Con 9))
  
ex2 = If (Var 'h') (Assgn 'l' (Con 5))
                   (Assgn 'l' (Con 9))
      
ex3 = Seq ex1 (Assgn 'l' (Con 1))
-}


-- Preguntas: Es LIO modelable como una free monad?
--            Tratar de tener el funtor Local s (s -> a)  
--            Propiedades: monotonia del pc (taint)            
--             * no-write down: no side-effects below pc (guard)
--             * pensar propiedad del local 
--             * taint para current label is not the same 
--               as for references. References do not do the 
--               join.
