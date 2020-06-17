{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import qualified Client.ChillInstitute.API as CIAPI
import Client.ChillInstitute.Types
import qualified Client.PutIO.API as PutIO
import Client.PutIO.Types as PutIO
import Client.Util
import Control.Arrow ((&&&))
import Control.Concurrent.Async.Lifted
import Control.Concurrent.MVar.Lifted
import Control.Lens
import Control.Monad.Base
import Control.Monad.Except (liftEither)
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger.CallStack
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Aeson as JSON
import Data.Aeson.Casing
import qualified Data.Aeson.TH as JSON
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Char
import Data.Foldable (find, traverse_)
import Data.Functor (($>))
import qualified Data.Map.Strict as M
import Data.Monoid (First(..))
import Data.Proxy (Proxy)
import qualified Data.Text as T
import Data.Traversable (for)
import GHC.Generics
import Network.HTTP.Types.Status (Status(..))
import Network.Wai
import Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant
import Servant.Client
  ( ClientEnv
  , ClientError(..)
  , ClientM
  , ResponseF(..)
  , runClientM
  )

-- import Servant.Client.Core (FailureResponse(..))
import Servant.Server

downloadDirName :: String
downloadDirName = "Letterboxd Sources Download Dir"

data MovieChannelState
  = FoundMovieChannel MovieChannel
  | RequestingDownload
  | DownloadingMovie PutIO.TransferWithLink
  | DownloadedMovie String PutIO.TransferWithLink

data MovieStateSnapshot
  = MSSFindingChannels
  | MSSFoundChannels [MovieChannelState]

data MovieState
  = MSFindingChannels
  | MSFoundChannels (M.Map T.Text (MVar MovieChannelState))

data ServerEnv =
  ServerEnv
    { _sePutIODownloadDir :: PutIO.File
    , _sePutIOClientEnv :: ClientEnv
    , _seFileListCache :: MVar (Maybe [PutIO.File])
    , _seMovieStates :: MVar (M.Map T.Text (MVar MovieState))
    }

data MovieStateReq =
  MovieStateReq
    { _dmrName :: T.Text
    }
  deriving (Show, Eq, Generic)

type API
   = "movie_state" :> QueryParam' '[ Required] "name" T.Text :> Get '[ JSON] MovieStateSnapshot

type ServerHandler = ReaderT ServerEnv Handler

findMovieFromCI :: T.Text -> ServerHandler [MovieChannel]
findMovieFromCI movie_name = do
  clientEnv <- liftIO CIAPI.getClientEnv
  indexers <- callServantClient clientEnv CIAPI.getIndexers
  fmap mconcat $
    for (take 5 indexers) $ \(Indexer iId _) -> do
      callServantClient clientEnv $ CIAPI.getMovies movie_name iId

data PutIOErrorResponseExtra =
  PutIOErrorResponseExtra
    { _piereExistingId :: Maybe Integer
    , _piereStatus :: Maybe String
    }
  deriving (Show, Eq, Generic)

data PutIOErrorResponse =
  PutIOErrorResponse
    { _teerExtra :: Maybe PutIOErrorResponseExtra
    , _teerErrorType :: String
    , _teerErrorMessage :: String
    }
  deriving (Show, Eq, Generic)

tryToAddTransfer :: Integer -> T.Text -> ServerHandler Transfer
tryToAddTransfer parent_folder_id magnet_url = do
  downloadDirId <- PutIO._fId <$> asks _sePutIODownloadDir
  let reqBody = PutIO.AddTransferReq magnet_url (Just parent_folder_id) Nothing
  clientEnv <- asks _sePutIOClientEnv
  liftIO (runClientM (PutIO.addTransfer reqBody) clientEnv) >>= \case
    Left (FailureResponse _ (Response (Status 400 _) _ _ body))
      -- Transfer already exists.
     -> do
      case JSON.eitherDecode body of
        Right (PutIOErrorResponse (Just (PutIOErrorResponseExtra (Just tId) _)) "Alreadyadded" _msg) -> do
          let _err =
                "A transfer already exists with id " <>
                show tId <>
                ". This actually means that there is no enough space. We return the transfer regardless."
          -- Make this a warning message.
          PutIO.TransferResponse transfer <-
            callServantClient clientEnv (PutIO.getTransfer tId)
          pure transfer
        Right other -> throwError (err400 {errBody = LBS8.pack (show other)})
        Left err -> do
          liftIO $
            putStrLn $ "Couldn't parse 400 response from put.io:" <> show err
          throwError
            (err400
               { errBody =
                   LBS8.pack
                     ("Couldn't parse 400 response from put.io:" <> show err)
               })
    Left err
      -- Transfer already exists.
     -> do
      liftIO $ putStrLn $ "Unexpected error response from put.io: " <> show err
      throwError
        (err400
           { errBody =
               LBS8.pack ("Unexpected error response from put.io: " <> show err)
           })
    Right (PutIO.TransferResponse transfer) -> do
      liftIO $ putStrLn $ "Added transfer with id: " <> PutIO._tName transfer
      pure transfer

listFiles :: ServerHandler [PutIO.File]
listFiles = do
  clientEnv <- asks _sePutIOClientEnv
  cache <- asks _seFileListCache
  modifyMVar cache $ \case
    Nothing -> do
      liftIO $ putStrLn "Cache is empty, sending request to Put.oi."
      PutIO.FilesResponse files <-
        callServantClient clientEnv $
        PutIO.listFiles (Just (-1)) Nothing True True
      pure (Just files, files)
    Just files_cache -> do
      liftIO $ putStrLn "File list exists in the cache. We are using it."
      pure (Just files_cache, files_cache)

clearFileListCache :: ServerHandler ()
clearFileListCache = do
  liftIO $ putStrLn "Clearing the cache..."
  cache <- asks _seFileListCache
  void $ liftIO $ swapMVar cache Nothing

getMovieStateSnapshot :: MonadBase IO m => MovieState -> m MovieStateSnapshot
getMovieStateSnapshot MSFindingChannels = pure MSSFindingChannels
getMovieStateSnapshot (MSFoundChannels channels) =
  MSSFoundChannels <$> traverse readMVar (M.elems channels)

getMovieState :: T.Text -> ServerHandler MovieStateSnapshot
getMovieState name
  -- We don't directly "take" MVar to check existence of this entry, instead we first try to read it because that would cause every request to block on this single MVar, eventually causing overhead.
 = do
  movie_states_var <- asks _seMovieStates
  (M.lookup name <$> readMVar movie_states_var) >>= \case
    Just movie_state_var ->
      liftIO . getMovieStateSnapshot =<< readMVar movie_state_var
    Nothing -> do
      movie_state_var <-
        modifyMVar movie_states_var $ \movie_states ->
          case M.lookup name movie_states of
            Just movie_state_var -> do
              pure (movie_states, movie_state_var)
            Nothing -> do
              let new_movie_state = MSFindingChannels
              new_movie_state_var <- newMVar new_movie_state
              let new_movie_states =
                    M.insert name new_movie_state_var movie_states
              pure (new_movie_states, new_movie_state_var)
      (getMovieStateSnapshot =<< readMVar movie_state_var) <*
        async (findMovieChannel name movie_state_var)

findMovieChannel :: T.Text -> MVar MovieState -> ServerHandler ()
findMovieChannel name movie_state_var
   -- This is a very costly function, we return earlier if there are other threads that are modifying this movie's state.
 = do
  is_empty <- isEmptyMVar movie_state_var
  if is_empty
    then pure ()
      -- Similarly, we need to start modifying early on so that other threads see the empty state var and return earlier.
    else do
      modifyMVar movie_state_var $ \case
        MSFoundChannels channels -> do
          pure (MSFoundChannels channels, ())
        MSFindingChannels -> do
          channels <- find_movie_channels
          channel_vars <-
            for channels $ \c -> (_mId c, ) <$> newMVar (FoundMovieChannel c)
          pure (MSFoundChannels (M.fromList channel_vars), ())
  where
    find_movie_channels :: ServerHandler [MovieChannel]
    find_movie_channels = do
      liftIO $ putStrLn "Finding movie from CI..."
      findMovieFromCI name

downloadMovie :: T.Text -> ServerHandler [MovieChannelState]
downloadMovie name = do
  download_dir <- asks _sePutIODownloadDir
  (exists, movie_download_folder) <-
    createOrFindFolder (PutIO._fId download_dir) name
  if exists
    then do
      liftIO $
        putStrLn
          "Download directory for this movie already exists, finding transfers associated with this directory."
      transfers <-
        find_existing_movie_transfers (PutIO._fId movie_download_folder)
      traverse transfer_to_movie_channel_state transfers
    else do
      liftIO $ putStrLn "Finding movie from CI..."
      movies <- findMovieFromCI name
      liftIO $ putStrLn "Movies from  CI:"
      liftIO $ print movies
      transfers <-
        traverse
          (attach_link <=<
           tryToAddTransfer (PutIO._fId movie_download_folder) . _mLink)
          (take 20 movies)
      traverse transfer_to_movie_channel_state transfers
  where
    transfer_to_movie_channel_state ::
         PutIO.TransferWithLink -> ServerHandler MovieChannelState
    transfer_to_movie_channel_state twl@(PutIO.TransferWithLink _ (Just url)) =
      pure $ DownloadedMovie url twl
    transfer_to_movie_channel_state twl@(PutIO.TransferWithLink t@(PutIO.Transfer {_tPercentDone}) Nothing) =
      case _tPercentDone of
        Just 100 ->
          find_download_link t >>= \case
            Nothing -> pure $ DownloadingMovie twl
            Just url -> pure $ DownloadedMovie url twl
        _ -> pure $ DownloadingMovie twl
    find_existing_movie_transfers ::
         Integer -> ServerHandler [PutIO.TransferWithLink]
    find_existing_movie_transfers movie_dir_id = do
      clientEnv <- asks _sePutIOClientEnv
      PutIO.TransfersResponse transfers <-
        callServantClient clientEnv PutIO.listTransfers
      traverse attach_link $
        filter ((== movie_dir_id) . PutIO._tSaveParentId) transfers
    find_download_link :: Transfer -> ServerHandler (Maybe String)
    find_download_link t
      | Just file_id <- _tFileId t = do
        clientEnv <- asks _sePutIOClientEnv
        files <- listFiles
        getFirst <$> find_mp4_file_of_file_or_folder files file_id >>= \case
          Nothing -> do
            liftIO $
              putStrLn $
              "Couldn't find a mp4 file under the folder " <> show file_id
            pure Nothing
          Just mp4_file -> do
            PutIO.FileDownloadUrlResponse url <-
              callServantClient
                clientEnv
                (PutIO.getFileDownloadUrl (PutIO._fId mp4_file))
            pure (Just url)
      | otherwise = pure Nothing
    attach_link :: Transfer -> ServerHandler PutIO.TransferWithLink
    attach_link t = PutIO.TransferWithLink t <$> find_download_link t
    find_mp4_file_of_file_or_folder ::
         [PutIO.File] -> Integer -> ServerHandler (First PutIO.File)
    find_mp4_file_of_file_or_folder all_files file_id =
      case find ((== file_id) . PutIO._fId) all_files of
        Nothing -> do
          liftIO $ putStrLn $ "Couldn't find file with id: " <> show file_id
          pure $ First Nothing
          -- pure $ Left $ "Could not find the file with id " <> show file_id
        Just f@(PutIO.File {_fId, _fFileType}) ->
          case _fFileType of
            "VIDEO" -> pure $ First (Just f)
            "FOLDER" ->
              let children = filter ((== _fId) . PutIO._fParentId) all_files
                  children_ids = map PutIO._fId children
               in foldMapA
                    (find_mp4_file_of_file_or_folder all_files)
                    children_ids
            other -> do
              liftIO $ putStrLn $ "Unknown file type: " <> _fFileType
              pure $ First Nothing

throw500 :: String -> ServerHandler a
throw500 message = throwError (err500 {errBody = LBS8.pack message})

createOrFindFolder :: Integer -> T.Text -> ServerHandler (Bool, PutIO.File)
createOrFindFolder parent_folder_id name = do
  clientEnv <- asks _sePutIOClientEnv
  PutIO.FilesResponse files <-
    callServantClient clientEnv $
    PutIO.listFiles (Just parent_folder_id) (Just "FOLDER") True True
  case find ((== name) . T.pack . PutIO._fName) files of
    Nothing -> do
      PutIO.FileResponse file <-
        callServantClient clientEnv $
        PutIO.createFolder
          (PutIO.CreateFolderReq (T.unpack name) parent_folder_id)
      pure (False, file)
    Just f -> pure (True, f)

server :: ServerT API ServerHandler
server = getMovieState

callServantClient :: ClientEnv -> ClientM a -> ServerHandler a
callServantClient clientEnv clientM = do
  liftIO (runClientM clientM clientEnv) >>= \case
    Left err -> do
      liftIO $ print err
      throwError (err500 {errBody = LBS8.pack (show err)})
    Right movies -> pure movies

myApi :: Proxy API
myApi = Proxy

mkServerEnv :: T.Text -> ExceptT ClientError IO ServerEnv
mkServerEnv api_token = do
  _sePutIOClientEnv <- liftIO $ PutIO.getClientEnv api_token
  _sePutIODownloadDir <- find_download_dir _sePutIOClientEnv
  _seFileListCache <- newMVar Nothing
  _seMovieStates <- newMVar M.empty
  pure ServerEnv {..}
  where
    is_download_dir :: PutIO.File -> Bool
    is_download_dir PutIO.File {_fName, _fFileType} =
      _fName == downloadDirName && _fFileType == "FOLDER"
    find_download_dir :: ClientEnv -> ExceptT ClientError IO PutIO.File
    find_download_dir client_env = do
      PutIO.FilesResponse files <-
        ExceptT $
        runClientM (PutIO.searchFiles downloadDirName Nothing) client_env
      case find is_download_dir files of
        Nothing -> create_download_dir client_env
        Just f -> pure f
    create_download_dir :: ClientEnv -> ExceptT ClientError IO PutIO.File
    create_download_dir client_env = do
      PutIO.FileResponse file <-
        ExceptT $
        runClientM
          (PutIO.createFolder (PutIO.CreateFolderReq downloadDirName 0))
          client_env
      pure file

app :: ServerEnv -> Application
app env =
  allowCors $
  serve myApi $
  hoistServerWithContext myApi (Proxy @'[]) (flip runReaderT env) $ server

allowCors :: Middleware
allowCors = cors (const $ Just appCorsResourcePolicy)

appCorsResourcePolicy :: CorsResourcePolicy
appCorsResourcePolicy =
  simpleCorsResourcePolicy
    { corsMethods = ["OPTIONS", "GET", "PUT", "POST"]
    , corsRequestHeaders = ["Authorization", "Content-Type"]
    }

runServer :: T.Text -> IO ()
runServer putIOAPIToken = do
  runExceptT (mkServerEnv putIOAPIToken) >>= \case
    Left clientError -> fail $ show clientError
    Right serverEnv ->
      liftIO $ do
        putStrLn "Server is ready to serve..."
        Warp.run 8081 $ logStdoutDev $ app serverEnv

$(JSON.deriveJSON (aesonPrefix snakeCase) ''MovieStateReq)

$(JSON.deriveJSON (aesonPrefix snakeCase) ''PutIOErrorResponse)

$(JSON.deriveJSON (aesonPrefix snakeCase) ''PutIOErrorResponseExtra)

$(JSON.deriveJSON (aesonPrefix snakeCase) ''MovieStateSnapshot)

$(JSON.deriveJSON (aesonPrefix snakeCase) ''MovieChannelState)
