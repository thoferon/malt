module Main (main) where

import Malt.Middleware
import Malt.Request
import Malt.Response
import Malt.Router

wrapReq :: SRequest -> MiddlewareStack (SRequest, SResponse)
wrapReq r = return (r, SResponse 200 [] "")

rootIsFoo :: SMiddleware
rootIsFoo (req, resp) = case splitAt 4 (getPath req) of
    (folder, path) | folder == "foo/" -> Success (setPath path req, resp)
    _ -> InternalError

router :: SMiddleware
router = routesToMiddleware [(isPath "bar", welcomePage), (\_ -> True, notFound)]

welcomePage :: SMiddleware
welcomePage (req,_) = Success (req, SResponse 200 [] "Welcome on /foo/bar")

site :: SRequest -> MiddlewareStack (SRequest, SResponse)
site req = wrapReq req >>= rootIsFoo >>= router

main :: IO ()
main = undefined
