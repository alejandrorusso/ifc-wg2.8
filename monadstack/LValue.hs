{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
module LValue where

import Core
import Data.IORef

data FS
data FI

data Effect = R | W | RW

--data LValue fs l v = LValue { labelOfTCB :: l, value :: v }
data LValue fs l v = LValue (IORef (l,v))

newLValue :: Label l => l -> a -> IFC l (LValue fs l a)
newLValue l x = do 
  Core.guard l
  r <- Core.liftIO $ newIORef (l,x)
  return (LValue r)

class Labelled t where
  labelOf :: Label l => t l v -> IFC l l
  enforce :: Label l => Effect -> t l v -> IFC l ()
  
instance Labelled (LValue FS) where
  labelOf (LValue r) = do (l,_) <-  Core.liftIO (readIORef r)
                          taint l
                          return l
  enforce R lv = labelOf lv >> return ()
  enforce W (LValue r) = do pc <- Core.ask
                            Core.liftIO (modifyIORef r (\(_,v) -> (pc,v)))

instance Labelled (LValue FI) where
  labelOf (LValue r) = fmap fst $ Core.liftIO $ readIORef r
  enforce R lv = labelOf lv >>= taint
  enforce W lv = labelOf lv >>= Core.guard
  
{- Definition of bless -}

bless :: (Label l, Labelled (LValue fs))
      => Effect -> (v -> IO a) -> LValue fs l v -> IFC l a  
bless R  = secureLift [R]
bless W  = secureLift [W]
bless RW = secureLift [W,R]

secureLift :: (Label l, Labelled (LValue fs))
              => [Effect] -> (v -> IO a) -> LValue fs l v -> IFC l a
secureLift es f lv@(LValue r) = do (_,v) <- Core.liftIO (readIORef r)
                                   sequence_ (map (\e -> enforce e lv) es)
                                   Core.liftIO (f v)
