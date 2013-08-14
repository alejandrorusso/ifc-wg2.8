{-# LANGUAGE TypeOperators, MultiParamTypeClasses, GADTs, FlexibleInstances, OverlappingInstances #-}
module AlaCarte where

import Control.Monad.Free

mkFree :: Functor f => f a -> Free f a
mkFree fa = wrap (fmap return fa)

infixr 9 :+:
data (f :+: g) x = Inl (f x) | Inr (g x)

instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f (Inl x) = Inl (fmap f x)
  fmap f (Inr x) = Inr (fmap f x)
  
infixr 1 :<:
class (Functor sub, Functor sup) => sub :<: sup where
   inj :: sub a -> sup a
   prj :: sup a -> Maybe (sub a)
 
instance (Functor f) => f :<: f where
  inj = id
  prj = Just

instance (Functor f, Functor g) => f :<: (f :+: g) where
   inj = Inl
   prj (Inl x) = Just x
   prj _       = Nothing
 
instance (Functor g, Functor h, f :<: h) => f :<: (g :+: h) where
   inj = Inr . inj
   prj (Inr y) = prj y
   prj _       = Nothing

newtype VoidF a = VoidF (VoidF a)
 
ex_falso_voidF :: VoidF a -> b
ex_falso_voidF (VoidF x) = ex_falso_voidF x
 
instance Functor VoidF where
   fmap f v = ex_falso_voidF v

run :: Free VoidF a -> a
run = foldFree id ex_falso_voidF

without :: (x :+: xs) a -> Either (x a) (xs a)
without (Inl x) = Left x
without (Inr y) = Right y

removeStep :: Functor f => 
              Free (x :+: f) a 
              -> Free f (Either (x (Free (x :+: f) a)) a)
removeStep (Pure a) = Pure $ Right a
removeStep (Impure f) = case without f of
                              Left x -> Pure $ Left x
                              Right x -> Impure $ fmap removeStep x