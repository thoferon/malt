module Malt.Request (
  Request(..),
  Method(..),
  Path,
  Headers,
  Params,
  SRequest(..),
  isPath
  ) where

import Data.List(lookup)

class Request a where
  getMethod :: a -> Method
  getPath   :: a -> Path
  getHeader :: String -> a -> Maybe String
  getParam  :: String -> a -> Maybe String

  setMethod :: Method -> a -> a
  setPath   :: Path   -> a -> a
  setHeader :: String -> String -> a -> a
  setParam  :: String -> String -> a -> a

data Method = GET | POST | PUT | DELETE deriving(Show, Eq)
type Path = String
type Headers = [(String, String)]
type Params = [(String, String)]

data SRequest = SRequest Method Path Headers Params
                deriving(Show, Eq)

instance Request SRequest where
  getMethod      (SRequest method _ _ _)  = method
  getPath        (SRequest _ path _ _)    = path
  getHeader name (SRequest _ _ headers _) = lookup name headers
  getParam  name (SRequest _ _ _ params)  = lookup name params

  setMethod method (SRequest _ path headers params)   = SRequest method path headers params
  setPath   path   (SRequest method _ headers params) = SRequest method path headers params
  setHeader = undefined
  setParam  = undefined

isPath :: (Request req) => Path -> req -> Bool
isPath path = (== path) . getPath
