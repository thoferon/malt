module Malt.Middleware (
  MiddlewareStack(..),
  SMiddleware(..),
  notFound
  ) where

import Malt.Request
import Malt.Response

data MiddlewareStack a = InternalError | Success a
                         deriving(Show, Eq)

instance Monad MiddlewareStack where
  return = Success
  (>>=)  = joinMiddlewares

joinMiddlewares :: MiddlewareStack a -> (a -> MiddlewareStack b) -> MiddlewareStack b
joinMiddlewares (Success reqresp) f = f reqresp
joinMiddlewares InternalError     _ = InternalError

type SMiddleware = (SRequest, SResponse) -> MiddlewareStack (SRequest, SResponse)

notFound :: (Request a, Response b) => (a,b) -> MiddlewareStack (a,b)
notFound (req,resp) = Success (req, setCode 404 . setBody "404 Not Found" $ resp)
