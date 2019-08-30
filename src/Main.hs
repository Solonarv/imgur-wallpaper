{-# LANGUAGE BlockArguments #-}
module Main where

import Control.Applicative
import Data.Foldable

import Options.Applicative as Opts
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.FilePath
import System.Directory

import Config

data Mode
  = OneShot
  | Daemon
  | WriteDefaultCfg
  deriving (Eq, Ord, Show)

data Args = Args
  { argsCfgLoc :: FilePath
  , argsMode :: Mode
  }
  deriving (Eq, Ord, Show)

argsP :: FilePath -> Opts.Parser Args
argsP defCfgLoc = liftA2 Args
  do  option str
        (  short 'c'
        <> long "config"
        <> value defCfgLoc
        <> metavar "FILE"
        <> help "Location of the config file."
        )
  do  asum
        [ flag' OneShot
            (  short '1'
            <> long "once"
            <> help "Just change the wallpaper once. Default mode."
            )
        , flag' Daemon
            (  short 'd'
            <> long "daemon"
            <> help "Run in the background, changing the wallpaper periodically."
            )
        , flag' WriteDefaultCfg
            (  long "write-default-cfg" 
            <> help "Write out the default configuration and exit"
            )
        , pure OneShot
        ]

main :: IO ()
main = do
  defCfgLoc <- configLocEnv
  args <- execParser $
    info (argsP defCfgLoc <**> helper)
         (  fullDesc
         <> progDesc "Randomly pick a wallpaper from a collection of imgur.com images and albums."
         <> header "imgur-wallpaper"
         )
  case argsMode args of
    OneShot -> loadEnv (argsCfgLoc args) >>= updateWallpaper
    Daemon  -> loadEnv (argsCfgLoc args) >>= daemonLoop
    WriteDefaultCfg -> do
      createDirectoryIfMissing True (takeDirectory (argsCfgLoc args))
      Text.writeFile (argsCfgLoc args) =<< defaultConfigTxt
      putStrLn $ "Wrote default configuration to " <> argsCfgLoc args

loadEnv :: FilePath -> IO Env
loadEnv cfgPath = pure Env

updateWallpaper :: Env -> IO ()
updateWallpaper env = putStrLn "Updating wallpaper"

daemonLoop :: Env -> IO a
daemonLoop env = error "loop not implemented"

