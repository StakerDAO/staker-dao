{-# LANGUAGE NoRebindableSyntax #-}

module Client.Tezos.Core
  ( TzEnv
  , runTzEnv

  , Env(..)
  , NodeAddress(..)
  , Verbosity(..)
  , getTezosClientCmd
  , mkEnv

  , exec
  , execSilent
  , execSilentWithStdout
  ) where

import Prelude hiding (hPutStrLn, unlines)

import Control.Concurrent (forkIO)
import qualified Control.Exception as E
import Data.Aeson (FromJSON)
import Data.List (unlines)
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import System.Environment (lookupEnv)
import System.Environment (getEnvironment)
import System.Exit (ExitCode)
import System.IO (hFlush, hGetLine, hPutStrLn)
import System.Process
  (CreateProcess(..), StdStream(..), createProcess, proc, waitForProcess)




data NodeAddress = NodeAddress
  { naHost :: Text
  , naPort :: Natural
  }
  deriving stock Generic
  deriving anyclass FromJSON

data Env = Env
  { envTezosClientCmd :: Text
  , envNodeAddr :: NodeAddress
  , envSecure :: Bool
  , envVerbosity :: Verbosity
  }
  deriving stock Generic
  deriving anyclass FromJSON

getTezosClientCmd :: IO Text
getTezosClientCmd = lookupEnv "TEZOS_CLIENT" >>=
    maybe
      (fail "TEZOS_CLIENT enviromet variable is missing")
      (pure . T.pack)

mkEnv :: FilePath -> IO Env
mkEnv nodeAddressConfigPath =
  Yaml.decodeFileThrow @IO @Env nodeAddressConfigPath

type TzEnv a = ReaderT Env IO a

runTzEnv :: TzEnv a -> Env -> IO a
runTzEnv = runReaderT

-- Flattened
data Verbosity =
    Silent
  | ShowStdOut
  | ShowStdErr
  | ShowBoth
  deriving (Eq, Generic, FromJSON, Read)

exec :: Verbosity -> [Text] -> TzEnv (Text, Text)
exec verbosity args =
  execWithShell verbosity args id

execWithEnvVerb :: [Text] -> TzEnv (Text, Text)
execWithEnvVerb cmd = do
  verb <- asks envVerbosity
  exec verb cmd

execSilentWithStdout :: [Text] -> TzEnv Text
execSilentWithStdout args = fst <$> execWithEnvVerb args

execSilent :: [Text] -> TzEnv ()
execSilent = void . execWithEnvVerb

-- For this to work well, you SHALL link with `threaded` RTS!!!
-- Stdin is inherited!
callProcessWithReadStd
  :: Verbosity
  -> FilePath
  -> [String]
  -> Maybe [(String, String)]
  -> IO (ExitCode, String, String)
callProcessWithReadStd verbosity cmd args penv = do
  (_, Just hout, Just herr, p) <-
    createProcess $ (proc cmd args) { env = penv
                                    , std_out = CreatePipe
                                    , std_err = CreatePipe
                                    , delegate_ctlc = True}

  let
    geth h ho v f = do
      ec <- E.try @E.IOException (hGetLine h)
      case ec of
        Right c -> do
          if v
            then hPutStrLn ho c
            else pure ()
          geth h ho v (c : f)
        _ -> pure f
    geth' h ho v fInit = do
      lock <- newEmptyMVar
      forkIO $
        geth h ho v fInit >>= putMVar lock
      pure lock

  outlock <-
    geth' hout stdout
      (verbosity == ShowStdOut || verbosity == ShowBoth) []
  hFlush stdout
  errlock <- geth' herr stderr False []
  outs <- unlines . reverse <$> takeMVar outlock
  errs <- unlines . reverse <$> takeMVar errlock
  when (verbosity == ShowStdErr || verbosity == ShowBoth) $
    hPutStrLn stderr errs *> hFlush stderr
  ec <- waitForProcess p
  pure (ec, outs, errs)

execWithShell
  :: Verbosity
  -> [Text]
  -> (IO (String, String) -> IO (String, String))
  -> TzEnv (Text, Text)
execWithShell verbosity args shellTransform = do
  Env{envNodeAddr = NodeAddress{..}, ..} <- ask
  let outputShell = do
        let allargs =
              [ "-A", naHost
              , "-P", show naPort
              ] <> (if envSecure then ["-S"] else [])  <> args
        -- putTextLn $ "EXEC!: " <> envTezosClientCmd <> " " <> T.intercalate " " allargs
        e <- getEnvironment
        -- Ignore exit code ATM
        (_r, o, err) <-
          callProcessWithReadStd
            verbosity
            (T.unpack envTezosClientCmd)
            (map T.unpack allargs)
            (Just $ modenv e)
        -- putStr $ "RECV!: " ++ o
        return (o, err)
  liftIO $ bimap T.pack T.pack <$> shellTransform outputShell
  where
    modenv e = ("TEZOS_CLIENT_UNSAFE_DISABLE_DISCLAIMER", "YES") : e
