{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

module MoreMonads where

import           Control.Monad          (ap, liftM, when)
import           Control.Monad.Identity (Identity (..))
import           Data.Map               (Map)

instance MonadError e (Either e) where
    throwError :: e -> Either e a
    throwError = Left

    catchError :: Either e a -> (e -> Either e a) -> Either e a
    catchError (Left e)  handler = handler e
    catchError (Right a) _       = Right a

checkedDiv :: Int -> Int -> Either String Int
checkedDiv x y
    | y == 0    = throwError "division by zero"
    | otherwise = pure $ div x y

checkedDiv' :: Int -> Int -> Either String Int
checkedDiv' x y =
    catchError (checkedDiv x y) $ \_ -> pure 0

checkedDiv'' :: Int -> Int -> Either String (Maybe Int)
checkedDiv'' x y =
    catchError (Just <$> checkedDiv x y) $ \_ -> pure Nothing

fortyTwo :: Int
fortyTwo = runIdentity $ do
    let x = 6
        y = 7
    pure $ x * y

newtype Config = Config
    { cfgSigDigits :: Int
    } deriving Show

round' :: Int -> Double -> Double
round' digits x = (fromIntegral (floor (x * 10 ^ digits + 0.5) :: Integer) :: Double) / 10 ^ digits

myDiv :: Config -> Double -> Double -> Double
myDiv cfg x y = round' (cfgSigDigits cfg) $ x / y

myRecip :: Config -> Double -> Double
myRecip cfg = myDiv cfg 1

recips :: Config -> [Double] -> [Double]
recips cfg = map (myRecip cfg)

type M = Reader Config

roundM :: MonadReader Config m => Double -> m Double
roundM x = do
    cfg <- ask
    pure $ round' (cfgSigDigits cfg) x

myDivM :: MonadReader Config m => Double -> Double -> m Double
myDivM x y = roundM $ x / y

myRecipM :: MonadReader Config m => Double -> m Double
myRecipM = myDivM 1

recipsM :: MonadReader Config m => [Double] -> m [Double]
recipsM = mapM myRecipM

withDoublePrecision :: MonadReader Config m => m a -> m a
withDoublePrecision = local (\cfg -> cfg{cfgSigDigits = 2 * cfgSigDigits cfg})

class Monad m => MonadReader r m | m -> r where
    ask :: m r
    local :: (r -> r) -> m a -> m a

instance MonadReader r ((->) r) where
    ask = id
    local f m r = m $ f r

type Env = Map String Int

type EnvErr = ReaderT Env (Either String) -- newtype EnvErr {runEnvErr :: Env -> Either String a}

instance MonadError () Maybe where
    throwError :: () -> Maybe a
    throwError () = Nothing

    catchError :: Maybe a -> (() -> Maybe a) -> Maybe a
    catchError Nothing    h = h ()
    catchError x@(Just _) _ = x

withDouble :: (Num r, MonadReader r m) => m a -> m a
withDouble = local (2 *)

class Monad m => MonadReader' m where
    type EnvType m :: *
    ask' :: m (EnvType m)
    local' :: (EnvType m -> EnvType m) -> m a -> m a

instance MonadReader' (Reader r) where
    type EnvType (Reader r) = r
    ask' = ReaderT $ \r -> Identity r
    local' f m = ReaderT $ \r -> runReaderT m (f r)

instance MonadReader' ((->) r) where
    type EnvType ((->) r) = r
    ask' = id
    local' f m r = m $ f r

instance MonadReader' EnvErr where
    type EnvType EnvErr = Env
    ask' = ReaderT $ \env -> Right env
    local' f m = ReaderT $ \env -> runReaderT m $ f env

withDouble' :: (Num (EnvType m), MonadReader' m) => m a -> m a
withDouble' = local' (2 *)

class Monad m => MonadError e m | m -> e where
    throwError :: e -> m a
    catchError :: m a -> (e -> m a) -> m a

newtype ReaderT r m a = ReaderT {runReaderT :: r -> m a}

instance Monad m => Functor (ReaderT r m) where
    fmap :: (a -> b) -> ReaderT r m a -> ReaderT r m b
    fmap = liftM

instance Monad m => Applicative (ReaderT r m) where
    pure :: a -> ReaderT r m a
    pure a = ReaderT $ \_ -> pure a

    (<*>) :: ReaderT r m (a -> b) -> ReaderT r m a -> ReaderT r m b
    (<*>) = ap

instance Monad m => Monad (ReaderT r m) where
    return :: a -> ReaderT r m a
    return = pure

    (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
    m >>= k = ReaderT $ \r -> do -- do-notation in m!!!
        a <- runReaderT m r
        runReaderT (k a) r

instance Monad m => MonadReader r (ReaderT r m) where
    ask :: ReaderT r m r
    ask = ReaderT pure

    local :: (r -> r) -> ReaderT r m a -> ReaderT r m a
    local f m = ReaderT $ \r -> runReaderT m $ f r

type Reader r = ReaderT r Identity

instance MonadTrans (ReaderT r) where
    lift :: Monad m => m a -> ReaderT r m a
    lift m = ReaderT $ const m

runReader :: Reader r a -> r -> a
runReader m r = runIdentity $ runReaderT m r

class MonadTrans t where
    lift :: Monad m => m a -> t m a

instance MonadError e m => MonadError e (ReaderT r m) where
    throwError :: e -> ReaderT r m a
    throwError = lift . throwError

    catchError :: ReaderT r m a -> (e -> ReaderT r m a) -> ReaderT r m a
    catchError m h = ReaderT $ \r ->
        catchError (runReaderT m r) $ \e ->
            runReaderT (h e) r

foo :: ReaderT Int Maybe Double
foo = do
    n <- ask
    when (n == 0) $
        throwError ()
    pure $ 1 / fromIntegral n
