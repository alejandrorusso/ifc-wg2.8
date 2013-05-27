{-# LANGUAGE ScopedTypeVariables #-}
module CallCC where



data CPS r a = MkC { cont :: (a -> r) -> r } 


instance Monad (CPS r) where 
  return x = MkC $ \c -> c x   
  m >>= f  = MkC $ \c -> cont m (\a -> cont (f a) c) 


{- 
   Intro: 
   ------

   The idea of callCC is to bind (in this case using a lambda) the current 
   continuation. It is used something like this: 

   do callCC (\k -> do ...
                       ... 
                       k 5      -- Here the continuation at the time of calling 
                                   callCC (i.e. k) is giving as argument the number 5. 
                       some_commands_1 )
      some_commands_2

   What is k in the example above? Well, it is what continues after callCC, i.e., 
   some_comands_2!

   So, the idea is that all the instructions in the do-block before k 5 gets executed. 
   What happen when applied k inside callCC? Well, once that the continuation k is 
   applied some_commands_1 gets ignore (it does not execute) and the execution  
   "jumps out" and continue executing some_commands_2. This is the main feauture of 
   callCC!!! It is possible to have nested callCC to manipulate the control flow as 
   indicated above. 

   How to implement callCC
   -----------------------

   Seeing the implementation of callCC and its type is not trivial. This is mainly due to    
   the fact that we want to handle a continuation inside callCC (by doing \k) and 
   continuations are "hidden" under the monad at the same time. So, let's start seeing
   how we will use callCC and how we could implemented. We saw above a typical use:

   do callCC (\k -> do ...
                       ... 
                       k 5      -- Here the continuation at the time of calling 
                                   callCC (i.e. k) is giving as argument the number 5. 
                       some_commands_1 )
      some_commands_2


   Then, we could say that returns a CPS r a since it should execute any instruction  
   before k 5. So, let's say CPS r a. Then, we have:

   calCC: ..... -> CPS r a 

   callCC f = MkC $ \c -> ...... 

   Now, we see that callCC takes the continuation at the time of calling callCC. Clearly, 
   the current continuation is applied to an a (as above shows, to 5). Then, 

   calCC: ( a -> .... ) -> CPS r a 

   callCC f = MkC $ \c -> f (\a -> ...) .... 

   So, as said before, we need to apply the continuation at the time of calling callCC 
   to a. Then, 

   callCC f = MkC $ \c -> f (\a -> c a) ....  

   However, observe that the type of callCC become now

   callCC: ( a -> r ) -> CPS r a 

   However, r cannot be used in a do-expression!    
   
   callCC (\k -> do ...
                    ... 
                    k 5      
                    some_commands_1 )

   In this light, k must return a CPS type. However, recall that the type for CPS is  
   (_ -> r) -> r and we are interested that the final r is the one  
   produced by applying k 5! So, we write a computation that ignores the 
   continuation after k 5 (i.e. some_commands_1) and then just returns the r 
   produced by k 5!

   callCC: ( a -> CPS r b ) -> CPS r a 

   callCC f = MkC $ \c -> f (\a -> MkC (\_ -> c a)) ....  

   Are we done? Not yet. The function f take a function of the type a -> CPS r b, used  
   in the body of a do-expression and it returns the CPS r a that we were looking in the 
   beginning! So, the type is finally 

   callCC: ( (a -> CPS r b) -> CPS r a ) -> CPS r a 
   callCC f = MkC $ \c -> f (\a -> MkC (\_ -> c a)) c  
                                                   ^^^ This c is some_commands_2
                                       ^^^ This is some_commands_1
   
   From above, we can see that when applied the continuation some_commands_1 gets 
   ignore and instead called some_commands_2. 

   How do we realize that whatever is before k 5 gets executed? For instance, 

   callCC (\k -> do x <- return 1 
                    y <- return x + 1
                    k y      
                    some_commands_1 )
   
   Because what callCC returns the do-block. Therefore, 
   x <- return 1 >>= \y -> return x + 1 >>= k y gets executed! 


-}


callCC :: ( (a -> CPS r b) -> CPS r a ) -> CPS r a 

callCC f = MkC $ \c -> cont (f (\a -> MkC $ \_ -> c a)) c 


bar :: CPS r Int 
bar = callCC $ ( \(k :: (Int -> CPS r b)) -> do let n = 5 
                                                k n 
                                                return 25 )
                        
bar' = bar >>= \x -> return $ x+1      
t = cont bar' print 

