module Text.StateTransformer where

import Control.Monad

newtype StateT m s a = StateT (s -> m (s, a))

unit :: Monad m => a -> StateT m s a
unit a = StateT (\s -> return (s,a))

bind :: Monad m => StateT m s a -> (a -> StateT m s b) -> StateT m s b
bind (StateT t) f = 
    StateT $ \s -> let ma = t s 
                   in  ma >>= \(s', a) -> let (StateT f') = f a
                                          in  (f' s')

instance Monad m => Monad (StateT m s) where
    return = unit
    (>>=) = bind

zero :: MonadPlus m => StateT m s a
zero = StateT (\s -> mzero)

plus :: MonadPlus m => StateT m s a -> StateT m s a -> StateT m s a
plus (StateT a) (StateT b) = StateT $ \s -> (a s) `mplus` (b s) 

instance MonadPlus m => MonadPlus (StateT m s) where
    mzero = zero
    mplus = plus
