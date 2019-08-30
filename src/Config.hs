{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Config where

import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.Maybe
import System.Environment

import Control.Monad.Trans.Maybe
import Data.Ini.Config
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import FileEmbedLzma
import System.Directory
import System.FilePath

data TargetType = Album | Image
  deriving (Eq, Ord, Show)

data Target = Target
  { targetType :: TargetType
  , targetId :: Text
  , targetWeight :: Word
  }
  deriving (Eq, Ord, Show)

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
    , cfgCacheDir = cfgCacheDir cfg2
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
  pure (Cfg maxCache cacheDir (toList targets))
  where
    readTargetType s
      | s `elem` ["album", "a"] = Right Album
      | s `elem` ["image", "img", "i"] = Right Image
      | otherwise = Left ("invalid target type: '" <> Text.unpack s <> "', must be 'album' or 'image'")

errorIO :: String -> IO a
errorIO = ioError . userError

loadConfigFrom :: FilePath -> IO Cfg
loadConfigFrom = either errorIO pure . flip parseIniFile cfgParser <=< Text.readFile 

configLocEnv :: IO FilePath
configLocEnv = asum
  [ getEnvNN "IMGUR_WALLPAPER_CONFIG"
  , (</> "config.ini") <$> getXdgDirectory XdgConfig "imgur-wallpaper"
  ]

getEnvNN :: String -> IO String
getEnvNN k = do
  s <- getEnv k
  when (null s) $ errorIO $ "Environment variable '" <> k <> "' empty or not set!"
  pure s

defaultConfigTemplate :: Text
defaultConfigTemplate = $(embedText "data/default-config.ini")

defaultConfigTxt :: IO Text
defaultConfigTxt = do
  cacheDir <- asum
    [ getEnvNN "IMGUR_WALLPAPER_CACHE_DIR"
    , getXdgDirectory XdgCache "imgur-wallpaper"
    ]
  pure
    $ Text.replace "$DEFAULT_CACHEDIR" (Text.pack cacheDir)
    $ defaultConfigTemplate
  