module Main (main) where

import Malt.Middleware
import Malt.Request
import Malt.Response

wrapReq :: SRequest -> MiddlewareStack (SRequest, SResponse)
wrapReq r = return (r, SResponse 200 [] "")

rootIsFoo :: SMiddleware
rootIsFoo (req, resp) = case (splitAt 4 . getPath) req of
    (folder, path) | folder == "foo/" -> Success (setPath req path, resp)
    _ -> InternalError

router :: SMiddleware
router = return

site :: SRequest -> MiddlewareStack (SRequest, SResponse)
site req = wrapReq req >>= rootIsFoo >>= router

main :: IO ()
main = undefined
