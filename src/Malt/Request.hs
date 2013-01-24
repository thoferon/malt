module Malt.Request (
  Request(..),
  Method(..),
  Path,
  Headers,
  Params,
  SRequest(..)
  ) where

import Data.List(lookup)

class Request a where
  getMethod :: a -> Method
  getPath   :: a -> Path
  getHeader :: a -> String -> Maybe String
  getParam  :: a -> String -> Maybe String

  setMethod :: a -> Method -> a
  setPath   :: a -> Path -> a
  setHeader :: a -> String -> String -> a
  setParam  :: a -> String -> String -> a

data Method = GET | POST | PUT | DELETE deriving(Show, Eq)
type Path = String
type Headers = [(String, String)]
type Params = [(String, String)]

data SRequest = SRequest Method Path Headers Params
                deriving(Show, Eq)

instance Request SRequest where
  getMethod (SRequest method _ _ _) = method
  getPath   (SRequest _ path _ _) = path
  getHeader (SRequest _ _ headers _) name = lookup name headers
  getParam  (SRequest _ _ _ params)  name = lookup name params

  setMethod (SRequest _ path headers params) method = SRequest method path headers params
  setPath   (SRequest method _ headers params) path = SRequest method path headers params
  setHeader = undefined
  setParam  = undefined

