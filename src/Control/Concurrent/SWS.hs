{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Control.Concurrent.SWS where

import Control.Monad.IO.Class
import Control.Monad.Trans.Control (MonadBaseControl)
import UnliftIO.STM

data SWS a =
  SWS
    { _mssSnapshot :: TVar a
    , _mssState :: TMVar a
    }
  deriving (Eq)

{-# INLINE newSWS #-}
newSWS :: (MonadIO m, MonadBaseControl IO m) => a -> m (SWS a)
newSWS v = do
  atomically $ do
    _mssSnapshot <- newTVar v
    _mssState <- newTMVar v
    pure SWS {..}

{-# INLINE withSWS #-}
withSWS :: (MonadIO m, MonadBaseControl IO m) => SWS a -> (a -> m b) -> m b
withSWS (SWS ref var) f = do
  state <- atomically $ takeTMVar var
  result <- f state
  atomically $ writeTVar ref state
  pure result

{-# INLINE modifySWS_ #-}
modifySWS_ :: (MonadIO m, MonadBaseControl IO m) => SWS a -> (a -> m a) -> m ()
modifySWS_ (SWS ref var) f = do
  state <- atomically $ takeTMVar var
  newState <- f state
  atomically $ do
    writeTVar ref newState
    putTMVar var newState

{-# INLINE modifySWS #-}
modifySWS ::
     (MonadIO m, MonadBaseControl IO m) => SWS a -> (a -> m (a, b)) -> m b
modifySWS (SWS ref var) f = do
  state <- atomically $ takeTMVar var
  (newState, result) <- f state
  atomically $ do
    writeTVar ref newState
    putTMVar var newState
    pure result

{-# INLINE readSWS #-}
readSWS :: (MonadIO m, MonadBaseControl IO m) => SWS a -> m a
readSWS (SWS ref _) = do
  readTVarIO ref
