{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, TypeOperators, FlexibleContexts #-}
module Core where

import AlaCarte
import Control.Monad.Free 
import Data.Monoid

class Label l where
  bottom :: l
  top :: l
  lub :: l -> l -> l
  lrt :: l -> l -> Bool

instance Label l => Monoid l where
  mempty = bottom
  mappend = lub  
                       

{-- Functors representing effects --}

data ReadEffect l a where 
  Taint :: l -> a -> ReadEffect l a
  
instance Functor (ReadEffect l) where
  fmap f (Taint l a) = Taint l (f a)

data WriteEffect l a where
  Guard :: l -> a -> WriteEffect l a
  
instance Functor (WriteEffect l) where
  fmap f (Guard l a) = Guard l (f a)

data Env s a where
  Get :: (s -> a) -> Env s a
  Put :: s -> a -> Env s a
  
instance Functor (Env l) where  
  fmap f (Get g) = Get (f . g)
  fmap f (Put s a) = Put s (f a) 
  
data IOEffect a where 
  LiftIO :: IO a -> IOEffect a

instance Functor IOEffect where
  fmap f (LiftIO m) = LiftIO (fmap f m)


{-- Data types à la carte --}
  
inject :: (g :<: f) => g (Free f a) -> Free f a
inject = Impure . inj

taint :: (ReadEffect l :<: f) => l -> Free f ()
taint l = inject (Taint l (Pure ()))

guard :: (WriteEffect l :<: f) => l -> Free f ()
guard l = inject (Guard l (Pure ()))

ask :: (Env s :<: f) => Free f s
ask = inject (Get Pure)

put :: (Env s :<: f) => s -> Free f ()
put s = inject (Put s (Pure ()))

liftIO :: (IOEffect :<: f) => IO a -> Free f a 
liftIO m = inject (LiftIO (fmap Pure m))

-- bless :: ((SafeIOEffects l :+: ReadEffect l :+: WriteEffect l) :<: f)
--          => Effect -> (v -> IO a) -> LValue l v -> Free f a
-- bless e g lv = inject (Bless e (fmap Pure . g) lv)

{-- IFC monad cares about writing and reading effects as well as 
    scope, i.e, environments --}
type IFC l a = Free (WriteEffect l :+: ReadEffect l :+: Env l :+: IOEffect) a

-- {-- Definition of "local" --}
local :: forall l a . Label l => IFC l () -> IFC l ()
local m =
  do (s :: l) <- Core.ask
     m
     Core.put s
     return ()

{-- Execution algebra --}
class (Functor f) => Run fl f where
  runAlg :: f (fl -> IO (a, fl)) -> fl -> IO (a,fl)

instance Label l => Run l (ReadEffect l) where
  runAlg (Taint l f) fl = f (fl `lub` l)

instance Label l => Run l (WriteEffect l) where
  runAlg (Guard l f) fl | fl `lrt` l = f fl
                        | otherwise  = fail "IFC violation!"

instance Label l => Run l (Env l) where
  runAlg (Get f)   fl = f fl fl
  runAlg (Put s f) _  = f s
              
instance (Run fl f, Run fl g) => Run fl (f :+: g) where
  runAlg (Inl x) = runAlg x
  runAlg (Inr x) = runAlg x

instance Run fl IOEffect where
  runAlg (LiftIO m) fl = m >>= ($ fl)

--runIFC :: Run l f => Free f a -> l -> IO (a, l)
--runIFC = foldFree (\x fl -> return (x,fl)) runAlg


{-- Translations --}
data TP = L | H
        deriving (Eq, Ord, Show)

instance Label TP where
  bottom = L
  top = H
  lub L L = L
  lub _ _ = H
  lrt H L = False
  lrt _ _ = True

{-
{-- Simplified LIO to IFC --}
data Labeled l a = MkLabel l a
     deriving (Eq, Ord, Show)

{- Simplified version of LIO -}
data LIO l a where
     Return  :: a -> LIO l a
     Bind    :: LIO l a -> (a -> LIO l b) -> LIO l b 
     Unlabel :: Labeled l a -> LIO l a 
     Label   :: l -> a -> LIO l (Labeled l a)
     ToLbl   :: l -> LIO l a -> LIO l (Labeled l a)

instance Monad (LIO l) where
  return = Return
  (>>=) = Bind


lioSem :: forall l a. Label l => LIO l a -> IFC l a 
lioSem (Return x) = return x
lioSem (Bind m f) = lioSem m >>= lioSem . f
lioSem (Unlabel (MkLabel l x)) = taint l >> return x
lioSem (Label l x) = Core.guard l >> return (MkLabel l x)
-- I cannot return a value yet! I need to add labeled objects and then build the toLabeled based on 
-- that. TODO! 
-- lioSem (ToLbl l m) = local (lioSem m)  
                     
                        
-- Examples
env1 :: [(Char, (Int, TP))]
env1 = [('h',(5,H)), ('l',(5,L))]  

-- Floating label H, result 2
exLIO  = do x <- Unlabel (MkLabel H 2)
            return x
           
-- Fail!
exLIO2 = do x <- Unlabel (MkLabel H 2)
            Label L x 

exLIO3 = do x <- return 1
            y <- ToLbl H $ do v <- Unlabel (MkLabel H 2)
                              return (v+x)
            z <- Unlabel y
            return z

exLIO4 = do ToLbl H $ Unlabel (MkLabel H 2)
            Label L 1
              
              
--runEx :: LIO TP a -> IO (a, TP)
--runEx m = runIFC (lioSem m) L


{-
Denning-style encoding. Some details to work on. 

type Name = Char
type Env l = [(Name, (Int, l))] 

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
-}