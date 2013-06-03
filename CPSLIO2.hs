{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module CPSLIO where

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

currentPC :: CPS r PC -- (PC -> a -> PC) -> PC 
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


lioCPS :: LIO a -> CPS r a 
lioCPS (Return x) = return x 

lioCPS (Bind m f) = do x <- lioCPS m  
                       lioCPS (f x)
                       
lioCPS (Unlabel (MkLabel l x)) = MkCC $ \pc -> \k -> k (join pc l) x  

