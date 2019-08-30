module Env where

import Data.ByteString (ByteString)
import Network.HTTP.Manager

data Env = Env
  { envHttpManager :: Manager
  }

runReqEnv :: Env -> Req a -> IO a
runReqEnv env = runReq defaultHttpConfig{ httpConfigAltManager = Just (envHttpManager env) }