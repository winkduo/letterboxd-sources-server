{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Control.Concurrent.SWS where

import Control.Concurrent.MVar.Lifted
import Control.Monad.IO.Class
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Proxy
import UnliftIO.STM

data SWS' (mode :: SWSMode) a =
  SWS
    { _mssSnapshot :: TVar a
    , _mssState :: TMVar a
    , _mssStopVar :: MVar ()
    }
  deriving (Eq)

-- | This data type is intended for kind-level.
data SWSMode
  = SWSModeNormal
  | SWSModeStoppable

type SWS = SWS' 'SWSModeNormal

type StoppableSWS = SWS' 'SWSModeStoppable

type family ModeModifier (mode :: SWSMode) (v :: k) :: k where
  ModeModifier 'SWSModeNormal a = a
  ModeModifier 'SWSModeStoppable a = Maybe a

class Mode (v :: SWSMode) where
  actWithMode :: MonadBaseControl IO m => MVar () -> m a -> m (ModeModifier v a)

instance Mode 'SWSModeNormal where
  {-# INLINE actWithMode #-}
  actWithMode _ action = action

instance Mode 'SWSModeStoppable where
  {-# INLINE actWithMode #-}
  actWithMode var action =
    tryReadMVar var >>= \case
      Nothing -> Just <$> action
      Just () -> pure Nothing

{-# INLINE stopSWS #-}
stopSWS ::
     (MonadIO m, MonadBaseControl IO m) => SWS' 'SWSModeStoppable a -> m Bool
stopSWS (SWS ref var stop_var) = tryPutMVar stop_var ()

-- | Send the stop message to a stoppable SWS. This will prevent further changes to the state.
-- Normally, preventing further "write" operations doesn't prevent state from changing, as there might be ongoing writes. That's why we also block on a read operation to ensure that state will be final when stopSWS returns.
{-# INLINE stopSWS_ #-}
stopSWS_ ::
     (MonadIO m, MonadBaseControl IO m) => SWS' 'SWSModeStoppable a -> a -> m ()
stopSWS_ (SWS ref var stop_var) val = do
  _ <- atomically $ takeTMVar var
  _ <- tryPutMVar stop_var ()
  atomically $ do
    writeTVar ref val
    putTMVar var val

{-# INLINE newSWS #-}
newSWS :: (MonadIO m, MonadBaseControl IO m) => a -> m (SWS' mode a)
newSWS v = do
  _mssStopVar <- newEmptyMVar
  atomically $ do
    _mssSnapshot <- newTVar v
    _mssState <- newTMVar v
    pure SWS {..}

{-# INLINE withSWS #-}
withSWS ::
     (MonadIO m, MonadBaseControl IO m) => SWS' mode a -> (a -> m b) -> m b
withSWS (SWS ref var _) f = do
  state <- atomically $ takeTMVar var
  result <- f state
  atomically $ writeTVar ref state
  pure result

{-# INLINE modifySWS_ #-}
modifySWS_ ::
     forall m mode a. (MonadIO m, MonadBaseControl IO m, Mode mode)
  => SWS' mode a
  -> (a -> m a)
  -> m ()
modifySWS_ (SWS ref var stop_var) f = do
  _ <-
    actWithMode @mode stop_var $ do
      state <- atomically $ takeTMVar var
      newState <- f state
      atomically $ do
        writeTVar ref newState
        putTMVar var newState
  pure ()

{-# INLINE modifySWS #-}
modifySWS ::
     forall m a b mode. (MonadIO m, MonadBaseControl IO m, Mode mode)
  => SWS' mode a
  -> (a -> m (a, b))
  -> m (ModeModifier mode b)
modifySWS (SWS ref var stop_var) f = do
  actWithMode @mode stop_var $ do
    state <- atomically $ takeTMVar var
    (newState, result) <- f state
    atomically $ do
      writeTVar ref newState
      putTMVar var newState
      pure result

{-# INLINE readSWS #-}
readSWS :: (MonadIO m, MonadBaseControl IO m) => SWS' mode a -> m a
readSWS (SWS ref _ _) = do
  readTVarIO ref
