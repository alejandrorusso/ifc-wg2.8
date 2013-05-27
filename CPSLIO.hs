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


interp :: PC -> Cmd -> Env -> Env 

interp pc (Assgn x e) env | lve <= s = (x, (eval e env,s)) : env
                          | otherwise = error "IFC violation!"
                          
                          where s = snd $ fromJust $ lookup x env 
                                lve = join (level e env) pc 

interp pc (Seq c1 c2) env = interp pc c2 (interp pc c1 env)

interp pc (If e c1 c2) env | eval e env /= 0 = interp (join pc (level e env)) c1 env
                           | otherwise       = interp (join pc (level e env)) c2 env     



high = 1
low  = 0

ex1 = If (Var 'h') (Assgn 'h' (Con 5))
                   (Assgn 'h' (Con 9))
  
ex2 = If (Var 'h') (Assgn 'l' (Con 5))
                   (Assgn 'l' (Con 9))

env1 :: [(Name, (Int, PC))]
env1 = [('h',(5,high)), ('l',(5,low))]  

interpcps :: PC -> Cmd -> Env -> (Env -> Env) -> Env 

interpcps pc (Assgn x e) env k | lve <= s = k $ (x, (eval e env,s)) : env   
                               | otherwise = error "IFC violation!"

                          where s = snd $ fromJust $ lookup x env 
                                lve = join (level e env) pc 
                                
interpcps pc (Seq c1 c2) env k = interpcps pc c1 env (\env' -> interpcps pc c2 env' k)

interpcps pc (If e c1 c2) env k 
     | eval e env /= 0 = interpcps (join pc (level e env)) c1 env k
     | otherwise       = interpcps (join pc (level e env)) c2 env k
                         
                         

tryCPS1 = interpcps 0 ex1 env1 id 


data CPS a = MkC { cont :: (a -> Env) -> Env }

instance Monad CPS where 
  return x = MkC $ \c -> c x   
  m >>= f  = MkC $ \c -> cont m (\a -> cont (f a) c) 


interCPS :: PC -> Cmd -> Env -> CPS Env 
interCPS pc (Assgn x e) env | lve <= s = return $ (x, (eval e env,s)) : env   
                            | otherwise = error "IFC violation!"
                                            
                          where s = snd $ fromJust $ lookup x env 
                                lve = join (level e env) pc 

interCPS pc (Seq c1 c2) env = do env' <- interCPS pc c1 env 
                                 interCPS pc c2 env'
                                 

interCPS pc (If e c1 c2) env 
     | eval e env /= 0 = interCPS (join pc (level e env)) c1 env 
     | otherwise       = interCPS (join pc (level e env)) c2 env 


---- Until here, pretty standard! 

interCPS' :: Cmd -> Env -> PC -> CPS Env 
interCPS' (Assgn x e) env = \pc -> let lve = join (level e env) pc  
                                       s   = snd $ fromJust $ lookup x env in 
                                   if (lve <= s ) 
                                      then return $ (x, (eval e env,s)) : env   
                                      else error "IFC violation!"
                                           
interCPS' (Seq c1 c2) env = \pc -> do env' <- interCPS' c1 env pc 
                                      interCPS' c2 env' pc 
                                 

interCPS' (If e c1 c2) env = \pc ->  
      if (eval e env /= 0) 
         then interCPS' c1 env (join pc (level e env))
         else interCPS' c2 env (join pc (level e env))



data CPSS a = MkCC { contt :: PC -> (a -> Env) -> Env }

instance Monad CPSS where 
  return x = MkCC $ \pc -> \k -> k x   
  m >>= f  = MkCC $ \pc -> \k -> contt m pc (\a -> contt (f a) pc k) 

currentPC :: CPSS PC 
currentPC = MkCC $ \pc -> \k -> k pc  

interCPS'' :: Cmd -> Env -> CPSS Env 
interCPS'' (Assgn x e) env = do pc <- currentPC    
                                
                                let lve = join (level e env) pc  
                                    s   = snd $ fromJust $ lookup x env  
                                    
                                if (lve <= s ) 
                                   then return $ (x, (eval e env,s)) : env   
                                   else error "IFC violation!"
                                           
interCPS'' (Seq c1 c2) env = do env' <- interCPS'' c1 env  
                                interCPS'' c2 env'  
                                 
interCPS'' (If e c1 c2) env = 
          do pc <- currentPC 
             withPC (join pc (level e env)) 
                    (if (eval e env /= 0) then (interCPS'' c1 env) 
                                          else (interCPS'' c2 env)) 

                                  
withPC :: PC -> CPSS a -> CPSS a 
withPC l (MkCC c) = MkCC $ \pc -> \k -> c l k  

tryCPS'' = contt (interCPS'' ex2 env1) 0 id 