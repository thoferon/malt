module Malt.Response (
  Response,
  Code,
  Headers,
  Body,
  SResponse(..)
  ) where

import Data.List(lookup)

class Response a where
  getCode   :: a -> Code
  getHeader :: a -> String -> Maybe String
  getBody   :: a -> Body

  setCode   :: a -> Code -> a
  setHeader :: a -> String -> String -> a
  setBody   :: a -> Body -> a

type Code    = Int
type Headers = [(String, String)]
type Body    = String

data SResponse = SResponse Code Headers Body
                 deriving(Show, Eq)

instance Response SResponse where
  getCode   (SResponse code _ _) = code
  getHeader (SResponse _ headers _) name = lookup name headers
  getBody   (SResponse _ _ body) = body

  setCode   (SResponse _ headers body) code = SResponse code headers body
  setHeader = undefined
  setBody   (SResponse code headers _) body = SResponse code headers body
