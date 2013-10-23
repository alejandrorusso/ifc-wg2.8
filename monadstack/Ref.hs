{-# LANGUAGE FlexibleContexts #-}
module Ref where

import Core
import LValue
import Data.IORef

type Ref fs l a = LValue fs l (IORef a)


newRef :: (Label l, Labelled (LValue fs)) 
          => l -> a -> IFC l (Ref fs l a)
newRef l x = do r <- Core.liftIO (newIORef x)
                newLValue l r 


readRef :: (Label l, Labelled (LValue fs)) 
           => Ref fs l a ->  IFC l a
readRef lv = bless R readIORef lv  


writeRef :: (Label l, Labelled (LValue fs)) 
            => Ref fs l a -> a -> IFC l () 
writeRef lv x = bless W ( ($x).writeIORef ) lv 
