{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module CPSLIO2 where

import Data.Maybe 

type Name = Char 

type PC = Int 

join :: PC -> PC -> PC 
join 0 1 = 1 
join 1 0 = 1
join 1 1 = 1 
join 0 0 = 0

data Exp where 
     Con :: Int -> Exp 
     Add :: Exp -> Exp -> Exp 
     Var :: Name -> Exp 
     deriving Show

data Cmd where 
     Assgn :: Name -> Exp -> Cmd 
     Seq   :: Cmd -> Cmd -> Cmd 
     If    :: Exp -> Cmd -> Cmd -> Cmd 
     deriving Show
     
type Env = [(Name, (Int, PC))] 

level :: Exp -> Env -> PC 
level (Con _)     env = 0
level (Var v)     env = snd $ fromJust $ lookup v env
level (Add e1 e2) env | level e1 env + level e2 env > 0 = 1  
                      | otherwise = 0

eval :: Exp -> Env -> Int
eval (Con x) env     = x 
eval (Var x) env     = fst $ fromJust $ lookup x env
eval (Add e1 e2) env = eval e1 env + eval e2 env 



high = 1
low  = 0

ex1 = If (Var 'h') (Assgn 'h' (Con 5))
                   (Assgn 'h' (Con 9))
  
ex2 = If (Var 'h') (Assgn 'l' (Con 5))
                   (Assgn 'l' (Con 9))
      
ex3 = Seq ex1 (Assgn 'l' (Con 1))

env1 :: [(Name, (Int, PC))]
env1 = [('h',(5,high)), ('l',(5,low))]  


data CPS r a = MkCC { contt :: PC -> (PC -> a -> r) -> r }

instance Monad (CPS r) where 
  return x = MkCC $ \pc -> \k -> k pc x   
  m >>= f  = MkCC $ \pc -> \k -> contt m pc (\pc' -> \a -> contt (f a) pc' k) 

currentPC :: CPS r PC -- PC -> (PC -> a -> PC) -> PC 
currentPC = MkCC $ \pc -> \k -> k pc pc 
                                
bracketPC :: PC -> CPS r a -> CPS r a 
bracketPC l (MkCC c) = MkCC $ \pc -> \k -> c (join pc l) (\pc' -> \a -> k pc a) -- Ignores the pc comming after c finishes!

interCPS :: Cmd -> Env -> CPS Env Env  
interCPS (Assgn x e) env = do pc <- currentPC    
                              let lve = join (level e env) pc  
                                  s   = snd $ fromJust $ lookup x env  
                                    
                              if (lve <= s ) 
                                    then return $ (x, (eval e env,s)) : env   
                                    else error "IFC violation!"
                                           
interCPS (Seq c1 c2) env = do env' <- interCPS c1 env  
                              interCPS c2 env'  
                                 
interCPS (If e c1 c2) env = 
          do pc <- currentPC 
             bracketPC (join pc (level e env)) 
                    (if (eval e env /= 0) then (interCPS c1 env) 
                                          else (interCPS c2 env)) 

                                  

run_example :: Cmd -> Env 
run_example ex = contt (interCPS ex env1) 0 (\pc -> \x -> x) 



data Labeled a = MkLabel PC a
     deriving Show

data LIO a where
     Return  :: a -> LIO a
     Bind    :: LIO a -> (a -> LIO b) -> LIO b 
     Unlabel :: Labeled a -> LIO a 
     Assign  :: Name -> b -> LIO ()


lioCPS :: LIO a -> CPS r a 

lioCPS (Return x) = return x 

lioCPS (Bind m f) = do x <- lioCPS m  
                       lioCPS (f x)
                       
lioCPS (Unlabel (MkLabel l x)) = MkCC $ \pc -> \k -> k (join pc l) x  


--
                                                     
lioCPS2 :: Env -> LIO a -> CPS r a 

lioCPS2 env (Return x) = return x

lioCPS2 env (Bind m f) = do x <- lioCPS2 env m  
                            lioCPS2 env (f x)
                       
lioCPS2 env (Unlabel (MkLabel l x)) = MkCC $ \pc -> \k -> k (join pc l) x  

lioCPS2 env (Assign n v) = do pc <- currentPC 
                              if (pc <= level (Var n) env) then return () -- Here, I don't do any effect since I don't model it
                              else error "IFC error in assignment!"


-- This example shows 3
exLIO = Bind (Return 1) (\x -> Bind (Unlabel (MkLabel high 2)) (\y -> Return (x+y))) 

-- This one should show an error!
exLIO2 = Bind (Return 1) (\x -> Bind (Unlabel (MkLabel high 2)) (\y -> Bind (Assign 'l' 1) (\_ -> Return (x+y)))) 

run_example2 :: LIO a -> a 
run_example2 ex = contt (lioCPS2 env1 ex) 0 (\pc -> \x -> x) 

-- callCC
-- contt :: PC -> (PC -> a -> r) -> r 
callCC :: ( (a -> CPS r b) -> CPS r a ) -> CPS r a 
callCC f = MkCC $ \pc -> \k -> contt (f (\a -> MkCC $ \pc' -> \_ -> k pc' a)) pc k  



-- No calling k, not failing!
try :: CPS r Integer 
try = do callCC (\k -> do k 1
                          lioCPS2 env1 exLIO)
         lioCPS2 env1 (Assign 'l' 1)
         return 0
      
-- No calling k, but falling since the current label is high
try2 :: CPS r Integer 
try2 = do callCC (\k -> lioCPS2 env1 exLIO)
          lioCPS2 env1 (Assign 'l' 1)
          return 0
       
-------------------------------------
-------------------------------------
-- Discussion with Pablo (Opal Hotel)           
-------------------------------------
-------------------------------------
          
-- reader :: pc -> ((a -> r) -> r)
-- reader PC (CPS r a)

-- LIO ~
-- CPST (Reader PC) r a ~ (a -> Reader PC r) -> Reader PC r
--                      ~ (a -> PC -> r) -> PC ->  r

-- get, put
-- newIOREf, readIORef, writeIORef

-- Cont (Reader s) r a ~?~ Cont (State s) r a  


-------------------------------------
-------------------------------------
-- Exceptions
-------------------------------------
-------------------------------------


thrown :: e -> CPS r (Either a e) 
thrown e = return (Right e) 


catch :: CPS r (Either a e) -> (e -> CPS r (Either a e)) -> CPS r (Either a e)  
catch m hd = callCC (\ok -> do err <- callCC (\notOk -> do result <- m   
                                                           case result of 
                                                                Left a -> ok (Left a) 
                                                                Right e -> notOk e)
                               hd err)
