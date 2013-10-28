{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
module LValue where

import Core
import Data.IORef

data FS
data FI

data Effect = R | W | RW

--data LValue fs l v = LValue { labelOfTCB :: l, value :: v }
data LValue fs l v = LValue (IORef (l,v))

newLValue :: Lattice l => l -> a -> IFC l (LValue fs l a)
newLValue l x = do 
  Core.guard l
  r <- Core.liftIO $ newIORef (l,x)
  return (LValue r)

class Labelled t where
  labelOf :: Lattice l => t l v -> IFC l l
  enforce :: Lattice l => t l v -> Effect -> IFC l ()
  
instance Labelled (LValue FS) where
  labelOf (LValue r) = do (l,_) <-  Core.liftIO (readIORef r)
                          taint l
                          return l
  enforce lv R = labelOf lv >> return ()
  enforce (LValue r) W = do pc <- Core.ask    
                            Core.liftIO (modifyIORef r (\(_,v) -> (pc,v)))

instance Labelled (LValue FI) where
  labelOf (LValue r) = fmap fst $ Core.liftIO $ readIORef r
  enforce lv R = labelOf lv >>= taint
  enforce lv W = labelOf lv >>= Core.guard
  
{- Definition of bless -}
bless :: (Lattice l, Labelled (LValue fs))
      => Effect -> (v -> IO a) -> LValue fs l v -> IFC l a  
bless R  = secureLift [R]
bless W  = secureLift [W]
bless RW = secureLift [W,R]

secureLift :: (Lattice l, Labelled (LValue fs))
              => [Effect] -> (v -> IO a) -> LValue fs l v -> IFC l a
secureLift es f lv@(LValue r) = do sequence_ $ map (enforce lv) es
                                   (_,v) <- Core.liftIO (readIORef r)
                                   Core.liftIO (f v)
