{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
module Config where

import Data.Foldable
import System.Environment

import Data.Ini.Config
import qualified Data.Text as Text
import System.FilePath

data TargetType = Album | Image

data Target = Target
  { targetType :: TargetType
  , targetId :: Text
  , targetWeight :: Word
  }

data Cfg = Cfg
  { cfgCacheMaxImgs :: Int
  , cfgCacheDir :: FilePath
  , cfgTargets :: [Target]
  }
  deriving (Eq, Ord, Show)

-- | Right-biased
instance Semigroup Cfg where
  cfg1 <> cfg2 = Cfg
    { cfgCacheMaxImgs = cfgCacheMaxImgs cfg2
    , cfgCacheDir = cfgCacheMaxImgs cfg2
    , cfgTargets = cfgTargets cfg1 <> cfgTargets cfg2
    }

cfgParser :: IniParser Cfg
cfgParser = do
  (maxCache, cacheDir) <- section "CACHE"
    $ liftA2 (,)
        do fieldDefOf "maximum-images" number 10
        do fieldOf    "directory" string
  targets <- sections "TARGET"
    $ liftA3 Target
        do fieldOf    "type" readTargetType
        do fieldOf    "id" string
        do fieldDefOf "weight" number 1
  pure (Target maxCache cacheDir (toList targets))
  where
    readTargetType s
      | s `elem` ["album", "a"] = Right Album
      | s `elem` ["image", "img", "i"] = Right Image
      | otherwise = Left ("invalid target type: '" <> s <> "', must be 'album' or 'image'")

errorIO :: String -> IO a
errorIO = ioError . userError

loadConfigFrom :: FilePath -> IO Cfg
loadConfigFrom = Text.readFile cfgPath >=> flip parseIniFile cfgParser >=> either errorIO pure

loadConfig :: IO Cfg
loadConfig = do
  configLocation <- asum
    [                                                            getEnvNN "IMGUR_WALLPAPER_CONFIG"
    , (              </> "imgur-wallpaper" </> "config.ini") <$> getEnvNN "XDG_CONFIG_HOME"
    , (</> ".config" </> "imgur-wallpaper" </> "config.ini") <$> getEnvNN "HOME"
    ]
  loadConfig configLocation

getEnvNN :: String -> IO String
getEnvNN k = do
  s <- getEnv k
  when (null s) (errorIO $ "getEnvNN: environment variable '" <> k <> "'is not defined!")
  pure s