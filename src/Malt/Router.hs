module Malt.Router (
  routesToMiddleware
  ) where

import Malt.Request
import Malt.Response
import Malt.Middleware

routesToMiddleware :: (Request a, Response b) => [(a -> Bool, (a, b) -> MiddlewareStack (a, b))] -> (a, b) -> MiddlewareStack (a, b)
routesToMiddleware [] _ = InternalError
routesToMiddleware ((matcher,handler):rest) reqResp@(req,resp) =
  if matcher req
  then handler reqResp
  else routesToMiddleware rest reqResp
