{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, TypeOperators, FlexibleContexts #-}
module Core where

import AlaCarte
import Control.Monad.Free 
import Data.Monoid
import Control.Monad.State as ST

class Label l where
  bottom :: l
  top :: l
  lub :: l -> l -> l
  lrt :: l -> l -> Bool

instance Label l => Monoid l where
  mempty = bottom
  mappend = lub  
                       

data IFC l a where 
  Return :: a -> IFC l a
  Taint  :: l -> IFC l a -> IFC l a 
  Guard  :: l -> IFC l a -> IFC l a   
  Get    :: (l -> IFC l a) -> IFC l a
  Put    :: l -> IFC l a -> IFC l a 
  LiftIO :: IO a -> (a -> IFC l b) -> IFC l b   -- Here, I need the binding explicitly. mmmm 

{-- IFC is a monad --}
instance Monad (IFC l) where 
  return          = Return 
  Return a    >>= f = f a
  Taint l m   >>= f = Taint l (m >>= f) 
  Guard l m   >>= f = Guard l (m >>= f)
  Get g       >>= f = Get (fmap (>>=f) g)
  Put l m     >>= f = Put l (m >>= f)
  LiftIO io g >>= f = LiftIO io (fmap (>>=f) g)

{-- Instance as a Functor for IFC. This is used in LValue module --}
instance Functor (IFC l) where
  fmap f (Return x)  = Return (f x)
  fmap f (Taint l m) = Taint l (fmap f m) 
  fmap f (Guard l m) = Guard l (fmap f m)
  fmap f (Get g)     = Get (fmap (fmap f) g)
  fmap f (Put l m)   = Put l (fmap f m)
  fmap f (LiftIO io g) = LiftIO io (fmap (fmap f) g)


taint :: l -> IFC l ()
taint l = Taint l $ return ()
          
guard :: l -> IFC l () 
guard l = Guard l $ return ()

ask ::  IFC l l 
ask = Get return 

put :: l -> IFC l () 
put l = Put l $ return ()

liftIO :: IO a -> IFC l a
liftIO m = LiftIO m return 

-- {-- Definition of "local" --}
local :: forall l a . Label l => IFC l () -> IFC l ()
local m =
  do (s :: l) <- Core.ask
     m
     Core.put s
     return ()


interIFC :: Label l => IFC l a -> StateT l IO a
interIFC (Return x)    = return x           
interIFC (Taint l m)   = do fl <- ST.get
                            ST.put (fl `lub` l) 
                            interIFC m
interIFC (Guard l m)   = do fl <- ST.get
                            when (not (fl `lrt` l)) $ fail "IFC violation!" 
                            interIFC m
interIFC (Get f)       = do fl <- ST.get
                            interIFC $ f fl
interIFC (Put l m)     = do ST.put l
                            interIFC m 
interIFC (LiftIO io f) = do a <- ST.liftIO io
                            interIFC (f a)

runIFC :: Label l => IFC l a -> IO (a,l)
runIFC m = runStateT (interIFC m) bottom


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
--            Propiedades: monotonia del pc (taint)            
--             * no-write down: no side-effects below pc (guard)
--             * pensar propiedad del local 
--             * taint para current label is not the same 
--               as for references. References do not do the 
--               join.
-}