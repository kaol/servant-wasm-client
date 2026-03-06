module Servant.Client.Internal.JS where

import           Data.Coerce
                 (coerce)
import           Data.Text
                 (Text, pack, unpack)
import           GHC.Wasm.Prim
                 (JSVal, JSString(..), toJSString, fromJSString)

-- ---------------------------------------------------------------------------
-- AbortController
-- ---------------------------------------------------------------------------

foreign import javascript unsafe
  "return new AbortController();"
  js_newAbortController :: IO JSVal

foreign import javascript unsafe
  "return $1.signal;"
  js_abortSignal :: JSVal -> IO JSVal

-- | Safe to call after the fetch has already settled — the browser ignores it.
foreign import javascript unsafe
  "$1.abort();"
  js_abort :: JSVal -> IO ()

-- ---------------------------------------------------------------------------
-- fetch()
-- ---------------------------------------------------------------------------

-- | Wrap a Haskell IO function into a JS-callable function (JSVal).
-- This is the WASM equivalent of foreign import ccall "wrapper" for C FunPtrs.
-- The resulting JSVal must be kept alive for as long as JS may call it.
foreign import javascript "wrapper"
  js_mkCallback :: (JSVal -> IO ()) -> IO JSVal

-- | Kick off a fetch() call.  Both callbacks are JS functions (JSVals created
-- with 'js_mkCallback') receiving a JSVal of the shape:
-- { status, statusText, headers: Headers, body: ArrayBuffer }.
-- statusText is captured before arrayBuffer() so it is available on both paths.
foreign import javascript unsafe
  "var __req = { method: $2, headers: $3, signal: $6 };\
  \if ($4 !== null) { __req.body = $4; }\
  \fetch($1, __req).then(function(r) {\
  \  var __status = r.status;\
  \  var __st = r.statusText;\
  \  var __hdrs = r.headers;\
  \  r.arrayBuffer().then(function(buf) {\
  \    var __cb = (__status >= 200 && __status < 300) ? $7 : $8;\
  \    __cb({ status: __status, statusText: __st, headers: __hdrs, body: buf });\
  \  });\
  \}).catch(function(e) {\
  \  $8({ status: 0, statusText: '', headers: new Headers(), body: new ArrayBuffer() });\
  \});"
  js_fetch
    :: JSVal   -- ^ URL
    -> JSVal   -- ^ method
    -> JSVal   -- ^ request Headers object
    -> JSVal   -- ^ body (ArrayBuffer) or null
    -> JSVal   -- ^ AbortSignal
    -> JSVal   -- ^ success callback (JS function)
    -> JSVal   -- ^ error callback (JS function)
    -> IO ()

-- ---------------------------------------------------------------------------
-- Response accessors
-- ---------------------------------------------------------------------------

foreign import javascript unsafe "return $1.status;"
  js_respStatus :: JSVal -> IO Int

foreign import javascript unsafe "return $1.statusText;"
  js_respStatusText :: JSVal -> IO JSVal

-- | Collect response headers into a flat JS array [k0, v0, k1, v1, ...].
-- Using forEach is the correct iteration protocol for Headers; building an
-- array avoids passing a Haskell function across the FFI boundary.
foreign import javascript unsafe
  "var __r = [];\
  \$1.headers.forEach(function(v, k) { __r.push(k); __r.push(v); });\
  \return __r;"
  js_headersToFlatArray :: JSVal -> IO JSVal

foreign import javascript unsafe "return $1.length;"
  js_arrayLength :: JSVal -> IO Int

foreign import javascript unsafe "return $1[$2];"
  js_arrayAt :: JSVal -> Int -> IO JSVal

-- | Read response.body as an ArrayBuffer (both success and error paths).
foreign import javascript unsafe "return $1.body;"
  js_respBody :: JSVal -> IO JSVal

-- ---------------------------------------------------------------------------
-- Request Headers
-- ---------------------------------------------------------------------------

foreign import javascript unsafe
  "return new Headers();"
  js_newHeaders :: IO JSVal

foreign import javascript unsafe
  "$1.append($2, $3);"
  js_headersAppend :: JSVal -> JSVal -> JSVal -> IO ()

-- ---------------------------------------------------------------------------
-- Misc
-- ---------------------------------------------------------------------------

foreign import javascript unsafe "return null;"
  js_null :: IO JSVal

-- ---------------------------------------------------------------------------
-- Haskell helpers
-- ---------------------------------------------------------------------------

-- | Build a JS Headers object from a list of (name, value) pairs.
buildHeaders :: [(Text, Text)] -> IO JSVal
buildHeaders pairs = do
  h <- js_newHeaders
  mapM_ (\(k, v) -> js_headersAppend h (unp k) (unp v)) pairs
  pure h
  where
    unp = coerce . toJSString . unpack

-- | Collect all headers from a response JSVal into a Haskell list.
collectHeaders :: JSVal -> IO [(Text, Text)]
collectHeaders resp = do
  arr <- js_headersToFlatArray resp
  len <- js_arrayLength arr
  let loop i acc
        | i < 0     = pure acc
        | otherwise = do
            k <- pack . fromJSString . coerce <$> js_arrayAt arr i
            v <- pack . fromJSString . coerce <$> js_arrayAt arr (i + 1)
            loop (i - 2) ((k, v) : acc)
  loop (len - 2) []
