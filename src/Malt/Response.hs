module Malt.Response (
  Response(..),
  Code,
  Headers,
  Body,
  SResponse(..)
  ) where

import Data.List(lookup)

class Response a where
  getCode   :: a -> Code
  getHeader :: String -> a -> Maybe String
  getBody   :: a -> Body

  setCode   :: Code -> a -> a
  setHeader :: String -> String -> a -> a
  setBody   :: Body -> a -> a

type Code    = Int
type Headers = [(String, String)]
type Body    = String

data SResponse = SResponse Code Headers Body
                 deriving(Show, Eq)

instance Response SResponse where
  getCode        (SResponse code _ _)    = code
  getHeader name (SResponse _ headers _) = lookup name headers
  getBody        (SResponse _ _ body)    = body

  setCode   code (SResponse _ headers body) = SResponse code headers body
  setHeader = undefined
  setBody   body (SResponse code headers _) = SResponse code headers body
