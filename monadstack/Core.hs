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
  with :: l -> l -> l  

instance Label l => Monoid l where
  mempty = bottom
  mappend = lub  
                       

data IFC l a where 
  Return :: a -> IFC l a
  Taint  :: l -> IFC l a -> IFC l a 
  Guard  :: l -> IFC l a -> IFC l a   
  Ask    :: (l -> IFC l a) -> IFC l a
  Local  :: IFC l () -> IFC l a -> IFC l a
  LiftIO :: IO a -> (a -> IFC l b) -> IFC l b   -- Here, I need the binding explicitly. mmmm 

{-- IFC is a monad --}
instance Monad (IFC l) where 
  return          = Return 
  Return a    >>= f = f a
  Taint l m   >>= f = Taint l (m >>= f) 
  Guard l m   >>= f = Guard l (m >>= f)
  Ask g       >>= f = Ask (fmap (>>=f) g)         -- Needed only for FS values. 
  Local m m'  >>= f = Local m (m' >>= f)
  LiftIO io g >>= f = LiftIO io (fmap (>>=f) g)

{-- Instance as a Functor for IFC. This is used in LValue module --}
instance Functor (IFC l) where
  fmap f (Return x)    = Return (f x)
  fmap f (Taint l m)   = Taint l (fmap f m) 
  fmap f (Guard l m)   = Guard l (fmap f m)
  fmap f (Ask g)       = Ask (fmap (fmap f) g)
  fmap f (Local m m')  = Local m (fmap f m') 
  fmap f (LiftIO io g) = LiftIO io (fmap (fmap f) g)


taint :: l -> IFC l ()
taint l = Taint l $ return ()
          
guard :: l -> IFC l () 
guard l = Guard l $ return ()

ask :: IFC l l 
ask = Ask return 

liftIO :: IO a -> IFC l a
liftIO m = LiftIO m return 

{-- Definition of "local" --}
local :: forall l a . Label l => IFC l () -> IFC l ()
local m = Local m $ return () 

{-- Priviligies (Core, not exported) 

This privileges is very general. Most of LIO only uses 
privileges for operations which are lifted. In our case, it will be 
for LiftIO. I like that withPrivileges only modifies taint and guard 
and it is defined homorphically for the rest. 

Having said that, does it make sense for Local?
--}

withPrivileges :: Label l => l -> IFC l a -> IFC l a 
withPrivileges p m@(Return x)  = m
withPrivileges p (Taint l m)   = Taint (with p l) $ withPrivileges p m
withPrivileges p (Guard l m)   = Guard (with p l) $ withPrivileges p m
withPrivileges p (Ask f)       = Ask (fmap (withPrivileges p) f)  
withPrivileges p (Local m m')  = Local (withPrivileges p m) (withPrivileges p m')   
withPrivileges p (LiftIO io f) = LiftIO io (fmap (withPrivileges p) f) 


{-- Interpretation of IFC into the State monad.
    We could perhaps interpret it into the reader monad pasing 
    a single reference as it is LIO implemented now
--}
interIFC :: Label l => IFC l a -> StateT l IO a
interIFC (Return x)    = return x           
interIFC (Taint l m)   = do fl <- ST.get
                            ST.put (fl `lub` l) 
                            interIFC m
interIFC (Guard l m)   = do fl <- ST.get
                            when (not (fl `lrt` l)) $ fail "IFC violation!" 
                            interIFC m
interIFC (Local m m')  = do fl <- ST.get
                            interIFC m
                            ST.put fl 
                            interIFC m'
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
  with H _ = L
  with _ l = l 
{-
--            Propiedades: monotonia del pc (taint)            
--             * no-write down: no side-effects below pc (guard)
--             * pensar propiedad del local 
--             * taint para current label is not the same 
--               as for references. References do not do the 
--               join.
-}