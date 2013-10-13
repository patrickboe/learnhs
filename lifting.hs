import Control.Monad hiding (liftM)
import Control.Monad.Trans.Class

liftM :: Monad m => (a1 -> r) -> m a1 -> m r
liftM f m = m >>= \x -> return $ f x

newtype ListT m a = ListT { runListT :: m [a] }

instance (Monad m) => Monad (ListT m) where
      return x = ListT $ return [x]
      tm >>= f = ListT $ runListT tm
                       >>= \xs -> mapM (runListT . f) xs
                                          >>= \yss -> return (concat yss)

instance MonadTrans ListT where
      lift m = ListT (m >>= return . (: []))
