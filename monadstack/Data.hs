{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Data where 

import Data.Maybe

data Expr f = In (f (Expr f))


data Val a = Val Int 
data Add a = Add a a 



data (f :+: g) a = Inl (f a) | Inr (g a) 

-- 10
example1 :: Expr (Val :+: Add)
example1 = In (Inl (Val 10))

-- 50 + 100
example2 :: Expr (Val :+: Add)
example2 = In (Inr (Add (In (Inl (Val 50))) (In (Inl (Val 100)))))

-- Evaluation --

instance Functor Val where
  fmap f (Val x) = Val x 
  
instance Functor Add where 
  fmap f (Add a a2) = Add (f a) (f a2)
  

instance (Functor td, Functor td2) => Functor (td :+: td2) where
  fmap f (Inl e) = Inl (fmap f e) 
  fmap f (Inr e) = Inr (fmap f e) 
  

foldExpr :: Functor d => (d a -> a) -> Expr d -> a 
foldExpr f (In t) = f (fmap (foldExpr f) t)  


class Functor d => Eval d where
   evalAlgebra :: d Int -> Int  
   
instance Eval Val where
   evalAlgebra (Val x) = x 
   
instance Eval Add where   
   evalAlgebra (Add i1 i2) = i1 + i2
   

instance (Eval d1, Eval d2) => Eval (d1 :+: d2) where
  evalAlgebra (Inl e) = evalAlgebra e
  evalAlgebra (Inr e) = evalAlgebra e
  

eval :: Eval f => Expr f -> Int 
eval expr = foldExpr evalAlgebra expr

-- Automatic injections
-- Observe that this is a general mechanism for any instance of Functor.
-- A short of "search in a list" but at the level of type. Along the search,
-- the injections are done accordingly (i.e., Inl and Inr).
class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub a -> sup a
  prj :: sup a -> Maybe (sub a) -- For pattern matching. Ignore now.  


instance Functor f => f :<: f where
  inj = id 
  prj = Just
  
  
instance (Functor f, Functor g) => f :<: (f :+: g) where
  inj = Inl
  
  -- Ignore so far
  prj (Inl e) = Just e
  prj (Inr _) = Nothing

instance (Functor f, Functor h, f :<: g) => f :<: (h :+: g) where
  inj = Inr . inj 
  
  -- Ignore so far
  prj (Inl _) = Nothing
  prj (Inr e) = prj e


inject :: (g :<: f) => g (Expr f) -> Expr f 
inject = In . inj 

val :: (Val :<: f) => Int -> Expr f 
val x = inject (Val x)

add :: (Add :<: f) => Expr f -> Expr f -> Expr f 
add e1 e2 = inject (Add e1 e2)


-- Adding multiplication

data Mul a = Mul a a  

instance Functor Mul where 
  fmap f (Mul e1 e2) = Mul (f e1) (f e2)
  
instance Eval Mul where
  evalAlgebra (Mul x y) = x * y
  

mul :: (Mul :<: f) => Expr f -> Expr f -> Expr f 
mul e1 e2 = inject (Mul e1 e2)


-- Render as a fold (not done in the paper)
class Functor d => Render d where
   renderAlgebra :: d String -> String  


instance Render Val where
   renderAlgebra (Val x) = show x
   
instance Render Add where
  renderAlgebra (Add s1 s2) = "( " ++ s1 ++ " + " ++ s2 ++ " )"
  
instance Render Mul where
  renderAlgebra (Mul s1 s2) = "( " ++ s1 ++ " * " ++ s2 ++ " )"

instance (Render f, Render g) => Render (f :+: g) where
  renderAlgebra (Inl e) = renderAlgebra e 
  renderAlgebra (Inr e) = renderAlgebra e

render :: Render f => Expr f -> String
render = foldExpr renderAlgebra 

-- It is important to associate to the right in order for the type-class trick in 
-- :<: finds it!
example3 :: Expr (Val :+: (Add :+: Mul))
example3 = mul (val 10) (add (val 20) (val 10)) 

-- To remove the associativity, that otherwise it is to the left, we introduce the 
-- type-operators extension
infixr 9 :+:

example4 :: Expr (Val :+: Add :+: Mul)
example4 = mul (val 10) (add (val 20) (val 10)) 



-- Render with open recursion
class RenderOR d where
  renderOR :: RenderOR f => d (Expr f) -> String
  
  
instance RenderOR Val where
  renderOR (Val x) = show x 
  

pretty :: RenderOR f => Expr f -> String 
pretty (In e) = renderOR e

-- Here, it is important to note that e1 and e2 are (Expr f), i.e., values of the form (In e). Therefore, 
-- I cannot just apply renderOR to them. I need to destruct them first, i.e., to call pretty.
instance RenderOR Add where
  renderOR (Add e1 e2) = "( " ++ pretty e1 ++ " + " ++ pretty e2 ++ " )"
  
instance RenderOR Mul where
  renderOR (Mul e1 e2) = "( " ++ pretty e1 ++ " * " ++ pretty e2 ++ " )"
  
instance (RenderOR f, RenderOR g) => RenderOR (f :+: g) where
  renderOR (Inl e) = renderOR e
  renderOR (Inr e) = renderOR e
  
-- Pattern matching. At this point, we construct nicely, and we define functions the "traverse" and expression. What about   
-- doing pattern matching?. Now look the definition of prj
  

-- The matching function: I will take an Expr f, e.g., something like Expr (Val :+: Add), and return the function that 
-- constructed it, i.e., Add (Expr (Val :+: Add)).
match :: (g :<: f) => Expr f -> Maybe (g (Expr f))
match (In e) = prj e


-- The constrains are here since we want to pattern match Mul and Add from the type f, so 
-- I need the prj from f to Mul and Add.
dist ::  (Mul :<: f, Add :<: f) => Expr f -> Maybe (Expr f)
dist e = do Mul a b <- match e
            Add c d <- match b
            return $ add (mul a c) (mul a d)
  

example5 :: Expr (Val :+: Add :+: Mul)
example5 = mul (val 10) (add (val 5) (val 1))

example6 = fromJust $ dist example5


example7 :: Expr (Val :+: Add :+: Mul)
example7 = add (val 10) (add (val 5) (val 1))

example8 = case dist example7 of  
                Nothing -> "Nada!"
                
-- Let implement a transformation that applies dist over an expression when possible. We need an algebra the transform 
-- an expression into another, i.e., f (Expr g) -> Expr g. The idea is that f :<: g.
class Functor f => Trans f where
     trans :: (f :<: g, Add :<: g, Mul :<: g, Trans g) => f (Expr g) -> Expr g  

instance Trans Val where
     trans (Val x) = val x 
     
instance Trans Add where
      trans (Add e1 e2) = add e1 e2
      
instance Trans Mul where
      -- Here, it is essential to call (mul e1 e2) :: Expr g and not  
      -- (Mul e1 e2) :: Mul (Expr g).
      trans (Mul e1 e2) =  case dist (mul e1 e2) of   
                             Nothing -> mul e1 e2
                             Just e  -> e
     

{---------------------------------------------------------------------

I cannot make it work. It asks for f :<: g when calling trans e, where 
e comes from (Inl e).

fix :: f a -> f a -> f a 
fix a b = a
instance (Trans f, Trans g) => Trans (f :+: g) where
 trans x@(Inl e) = case match (trans e) of  
                        Just e' -> inject $ fix (Inl (fix e' e)) x

----------------------------------------------------------------------}
                 

-- Monads  
                             

data Term f a = Pure a | Impure (f (Term f a))

instance Functor f => Functor (Term f) where
  fmap f (Pure a)   = Pure (f a)
  fmap f (Impure g) = Impure (fmap (fmap f) g) 

      
instance Functor f => Monad (Term f) where 
  return x = Pure x 
  
  (Pure x) >>= f   = f x
  (Impure f) >>= g = Impure (fmap (>>= g) f) 



data Incr t   = Incr Int t 
data Recall t = Recall (Int -> t) 
data Clear  t = Clear t

instance Functor Incr where
  fmap f (Incr n t) = Incr n (f t)
  
instance Functor Recall where
  fmap f (Recall t) = Recall (f . t)
  
instance Functor Clear where
  fmap f (Clear t) = Clear (f t)
  

injectT :: (f :<: g) =>  f (Term g a) -> Term g a 
injectT = Impure . inj

incr :: (Incr :<: g) => Int -> Term g () 
incr x = injectT (Incr x (Pure ()))

clear :: (Clear :<: g) => Term g () 
clear = injectT (Clear (Pure ()))

recall :: (Recall :<: g) => Term g Int
recall = injectT $ Recall Pure 


tick :: Term (Recall :+: Incr) Int
tick = do  n <- recall
           incr 1
           recall
           
clearOp :: Term (Clear :+: Incr) ()
clearOp = do incr 1
             clear
             
foldTerm :: Functor f =>  (a -> b) -> (f b -> b) -> Term f a -> b  
foldTerm base alg (Pure a)   = base a 
foldTerm base alg (Impure t) = alg (fmap (foldTerm base alg) t)

data Mem = Mem Int deriving Show

class Functor f => Run f where
  runAlgebra :: f (Mem -> (a, Mem)) -> Mem -> (a, Mem)
  
  
instance Run Incr where
  runAlgebra (Incr n f) (Mem x) = f (Mem (x + n))
  
-- Not in the paper. Just continues recursively, but where memory gets flushed!
instance Run Clear where
  runAlgebra (Clear f) (Mem x) = f (Mem 0)
  
instance Run Recall where
  runAlgebra (Recall f) (Mem x) = f x (Mem x)
  
  
instance (Run g, Run f) => Run (f :+: g) where
  runAlgebra (Inl e) = runAlgebra e
  runAlgebra (Inr e) = runAlgebra e
  

run :: Run f => Term f a -> Mem -> (a, Mem)
run = foldTerm base runAlgebra 
      where base = \a -> \m -> (a, m) -- This is the same as (,)
            


