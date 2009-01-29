> module Handlers where

> import Control.Monad.State                         (get, runStateT)
> import Control.Monad.Trans                         (lift, liftIO)
> import Data.Record.Label
> import Network.Protocol.Http                        hiding (hostname)
> import Network.Protocol.Uri
> import Network.Salvia.Httpd
> import Numeric                                      (showHex)
> import System.IO                                    (Handle, hFlush, hPutStrLn)
> import qualified Data.ByteString.Lazy   as L
> import qualified Data.Map as M

> sendChunked :: L.ByteString -> Handler ()
> sendChunked bs = do
>     enterM response $ setM (header "Transfer-Encoding") "chunked"
>     send (flip chunked bs)

> chunked         :: Handle -> L.ByteString -> IO ()
> chunked s bytes
>     | len == 0  = hPutStrLn s "0\r"
>     | otherwise = do
>         hPutStrLn s $ showHex len "\r"
>         L.hPut s hd
>         chunked s tl
>   where
>     (hd, tl) = L.splitAt 4096 bytes
>     len      = L.length hd

> hMultiPart             :: (t -> Handler (Maybe t)) -> t -> Handler ()
> hMultiPart worker init = do
>     ctx <- get
>     enterM response $ setM contentType ("multipart/x-mixed-replace; boundary=\"rn9012\"", Nothing)
>     sendStrLn "--rn9012"
>     send hFlush
>     send $ multipart ctx init
>   where
>     multipart ctx token s = do
>         (token', _) <- runStateT (hDefault' token) ctx
>         case token' of
>             Just t  -> multipart ctx t s
>             Nothing -> return ()
>     hDefault' token = do
>         reset
>         token' <- worker token
>         sendStrLn "--rn9012"
>         send hFlush
>         hPrinter'
>         return token'
>     hPrinter' = do
>         sendHeaders'
>         s <- getM sock
>         q <- getM queue
>         liftIO $ mapM_ ($ s) q
>     sendHeaders' = do
>         hs <- enterM response $ getM headers
>         s  <- getM sock
>         liftIO $ sendHeader' "Content-Type" hs s
>         liftIO $ putStrLn ""
>     sendHeader' hdr hs s = do
>         case M.lookup hdr hs of
>             Just val -> hPutStrLn s $ hdr ++ ": " ++ val
>             Nothing  -> return ()
