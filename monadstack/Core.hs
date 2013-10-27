{-# LANGUAGE GADTs, MultiParamTypeClasses #-}
module Core 
       
       (
        -- Lattice   
        Lattice (bottom, top, lub, lrt), 
        -- IFC Monad and operations
        IFC (),
        taint, 
        Core.guard,
        ask,
        local,
        Core.liftIO,
        -- Privileges
        Priv (rewrite),
        withPrivileges,
       )  
 
where

import Control.Monad.State as ST

class Lattice l where
  bottom :: l
  top :: l
  lub :: l -> l -> l
  lrt :: l -> l -> Bool

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
local :: Lattice l => IFC l () -> IFC l ()
local m = Local m $ return () 


{-- Priviligies  

This privileges is very general. Most of LIO only uses privileges for operations
which are lifted. In our case, it will be for LiftIO. withPrivileges only
modifies taint and when the floating label is observed (Ask). For the rest, it
is homorphically defined.

--}
class Lattice l => Priv p l where
  rewrite :: p -> l -> l 

-- Floating label is monotonic when ignored the first argument of Local!
withPrivileges :: Priv p l => p -> IFC l a -> IFC l a 
withPrivileges p m@(Return x)  = m
withPrivileges p (Taint l m)   = taint (rewrite p l) >> withPrivileges p m
withPrivileges p (Guard l m)   = Core.guard l >> withPrivileges p m  -- Do not change the label of the object
withPrivileges p (Ask f)       = ask >>= (withPrivileges p . f . rewrite p)   
withPrivileges p (Local m m')  = local (withPrivileges p m) >> withPrivileges p m' -- Check this  
withPrivileges p (LiftIO io f) = Core.liftIO io >>= fmap (withPrivileges p) f    

-- Lower the cleareance?
-- What about a GuardClr and the we can write withCleareance and lowerClr?
withCleareance :: Lattice l => l -> IFC l a -> IFC l a 
withCleareance c m@(Return x)  = m
withCleareance c (Taint l m)   = taint l >> Core.guard c >> withCleareance c m
withCleareance c (Guard l m)   = Core.guard l >> withCleareance c m
withCleareance c (Ask f)       = ask >>= fmap (withCleareance c) f
withCleareance c (Local m m')  = local (withCleareance c m) >> withCleareance c m'
withCleareance c (LiftIO io f) = Core.liftIO io >>= fmap (withCleareance c) f
                                

{-- Interpretation of IFC into the State monad.
    We could perhaps interpret it into the reader monad pasing 
    a single reference as it is LIO implemented now
--}
interpIFC :: Lattice l => IFC l a -> StateT l IO a
interpIFC (Return x)    = return x           
interpIFC (Ask f)       = do fl <- ST.get                              
                             interpIFC (f fl)
interpIFC (Taint l m)   = do fl <- ST.get  
                             ST.put (fl `lub` l) 
                             interpIFC m
interpIFC (Guard l m)   = do fl <- ST.get 
                             when (not (fl `lrt` l)) $ fail "IFC violation!" 
                             interpIFC m
interpIFC (Local m m')  = do fl <- ST.get
                             ST.liftIO $ runIFC m fl -- Here, it is implemented as an environment. So, there is only a put
                                                     -- in the interpretation. Easy to see monotonicity
                             interpIFC m'
interpIFC (LiftIO io f) = do a <- ST.liftIO io
                             interpIFC (f a)


runIFC :: Lattice l => IFC l a -> l -> IO (a,l)
runIFC m = runStateT (interpIFC m) 


{-
--            Propiedades: monotonia del pc (taint)            
--             * no-write down: no side-effects below pc (guard)
--             * pensar propiedad del local 
--             * taint para current label is not the same 
--               as for references. References do not do the 
--               join.
-}