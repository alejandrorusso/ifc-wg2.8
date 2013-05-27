{-# LANGUAGE GADTs #-}

module Inter where 

import Data.Maybe 


-- Defining standard interpreter

type Name = Char 

data Value where 
     Num :: Int -> Value 
     Fun :: (Value -> Value) -> Value 
     
instance Show Value where 
     show (Num n) = show n 
     show (Fun f) = show "Function!"


data Term where 
     Con :: Int  -> Term
     Var :: Name -> Term 
     Add :: Term -> Term -> Term 
     Lam :: Name -> Term -> Term
     App :: Term -> Term -> Term 
     deriving Show
     
type Env = [(Name, Value)] 


interp :: Term -> Env -> Value 
interp (Con x) env = Num x 

interp (Var n) env = fromJust $ lookup n env  -- Ignore erros

interp (Add t1 t2) env = Num (v1 + v2) 
                         where Num v1 = interp t1 env 
                               Num v2 = interp t2 env 

interp (Lam n t) env = Fun $ \a -> interp t ((n,a):env)

interp (App t1 t2) env = func val 
                         where Fun func = interp t1 env 
                               val = interp t2 env 

ex1 = App (Lam 'x' (Add (Var 'x') (Con 1))) (Con 5)



-- Continuaton passing style 

data Value' where 
     Num' :: Int -> Value' 
     -- I need to put the continuation inside the function type, why? 
     -- When evaluating the function, the body of it gets evaluated. 
     -- Therefore, a continuation is needed to indicate what to do after the body 
     -- gets evaluated with a value binded to the function variable!
     Fun' :: (Value' -> (Value' -> Answer) -> Answer) -> Value' 

instance Show Value' where 
     show (Num' n) = show n 
     show (Fun' f) = show "Function!"


type Answer = Value' 

type Env' = [(Name, Value')] 


interpcps :: Term -> Env' -> (Value' -> Answer) -> Answer  

interpcps (Con x) env = \c -> c (Num' x)

interpcps (Var n) env = \c -> c val 
                        where val = fromJust $ lookup n env
                              
interpcps (Add t1 t2) env = \c -> interpcps t1 env 
                                  (\num1 -> interpcps t2 env 
                                            (\num2 -> add num1 num2 c))


-- Observe that the function is waiting a continuation before it is applied!
interpcps (Lam x t) env = \c -> c $ Fun' $ \a -> interpcps t ((x,a):env) 

interpcps (App t1 t2) env = \c -> interpcps t1 env 
                                  (\func -> interpcps t2 env 
                                            (\val -> app func val c))


app :: Value' -> Value' -> (Value' -> Answer) -> Answer 
app (Fun' f) val = \c -> f val c 

add :: Value' -> Value' -> (Value' -> Answer) -> Answer 
add (Num' v1) (Num' v2) c = c (Num' (v1+v2))


run_ex1cps = interpcps ex1 [] id 

-- Continuation monad


data CPS a = MkC { cont :: (a -> Answer'') -> Answer'' } 


instance Monad CPS where 
  return x = MkC $ \c -> c x   
  m >>= f  = MkC $ \c -> cont m (\a -> cont (f a) c) 
  
data Value'' where 
     Num'' :: Int -> Value'' 
     Fun'' :: (Value'' -> CPS Value'') -> Value'' 

instance Show Value'' where 
     show (Num'' n) = show n 
     show (Fun'' f) = show "Function!"

type Answer'' = Value''

type Env'' = [(Name, Value'')] 


interpmcps :: Term -> Env'' -> CPS Value'' 

interpmcps (Con x) env = return (Num'' x)

interpmcps (Var n) env = return val 
                         where val = fromJust $ lookup n env
                              
interpmcps (Add t1 t2) env = do num1 <- interpmcps t1 env 
                                num2 <- interpmcps t2 env 
                                add' num1 num2
                               

-- Observe that the function is waiting a continuation before it is applied!
interpmcps (Lam x t) env = return $ Fun'' (\a -> interpmcps t ((x,a):env)) 

interpmcps (App t1 t2) env = do func <- interpmcps t1 env 
                                val  <- interpmcps t2 env 
                                app' func val 
                               

app' :: Value'' -> Value'' -> CPS Value'' 
app' (Fun'' f) val = f val  

add' :: Value'' -> Value'' -> CPS Value'' 
add' (Num'' v1) (Num'' v2) = return (Num'' (v1+v2))

run_ex1mcps = cont (interpmcps ex1 []) id 








