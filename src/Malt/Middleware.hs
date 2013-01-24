module Malt.Middleware (
  MiddlewareStack(..),
  SMiddleware(..)
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
