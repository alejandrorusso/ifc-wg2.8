{-# LANGUAGE FlexibleContexts #-}
module MVar where

import Core
import LValue
import Control.Concurrent.MVar as Conc
  
type MVar fs l a = LValue fs l (Conc.MVar a)


newEmptyMVar :: (Label l, Labelled (LValue fs)) 
                => l -> a -> IFC l (MVar.MVar fs l a)

newEmptyMVar l x = do r <- Core.liftIO Conc.newEmptyMVar
                      newLValue l r 


takeMVar :: (Label l, Labelled (LValue fs)) 
            => MVar.MVar fs l a ->  IFC l a
takeMVar lv = bless RW Conc.takeMVar lv  


putMVar :: (Label l, Labelled (LValue fs)) 
           => MVar.MVar fs l a -> a -> IFC l () 
putMVar lv x = bless RW ( ($x).Conc.putMVar ) lv 
