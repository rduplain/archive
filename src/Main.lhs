#! /usr/bin/env runhaskell

> module Main where

> import Codec.Archive.Tar
> import Codec.Archive.Zip
> import Control.Concurrent.STM                      (atomically, newTVar)
> import Control.Monad.State                         (get, runStateT)
> import Control.Monad.Trans                         (lift, liftIO)
> import Data.Maybe                                  (fromJust)
> import Data.Record.Label
> import Network.Protocol.Http                        hiding (hostname)
> import Network.Protocol.Uri
> import Network.Salvia.Handlers.Default              (hDefault)
> import Network.Salvia.Handlers.Error                (hError)
> import Network.Salvia.Handlers.ExtensionDispatcher  (hExtensionRouter)
> import Network.Salvia.Handlers.FileSystem           (hFileSystem)
> import Network.Salvia.Handlers.PathRouter           (hPrefixRouter)
> import Network.Salvia.Handlers.Session              (SessionHandler, mkSessions)
> import Network.Salvia.Httpd
> import Network.Socket                               (inet_addr)
> import System.Directory                             (setCurrentDirectory)
> import System.IO                                    (hFlush, hPutStrLn)
> import Text.StringTemplate
> import qualified Codec.Compression.GZip as G
> import qualified Codec.Compression.BZip as B
> import qualified Data.Map as M

> main = do
>     addr <- inet_addr "127.0.0.1"
>     cfg  <- defaultConfig
>     let cfg' = cfg {
>         hostname   = "localhost"
>       , email      = "nubgames@gmail.com"
>       , listenAddr = addr
>       , listenPort = 9000
>       }
>     counter  <- atomically $ newTVar 0
>     sessions <- mkSessions
>     start cfg' $ hDefault counter sessions handler

> handler   :: SessionHandler () ()
> handler _ = hPrefixRouter [
>       ("/download", downloadsHandler)
>     , ("",          hFileSystem "public")
>     ] $ hError NotFound

> downloadsHandler = hExtensionRouter [
>       (Nothing,     downloadHtml)
>     , (Just "bz2",  downloadTbz)
>     , (Just "html", downloadHtml)
>     , (Just "gz",   downloadTgz)
>     , (Just "zip",  downloadZip)
>     ] $ hError NotFound

> downloadHtml = do
>     source  <- liftIO $ readFile "templates/project_name.html"
>     project <- getProject
>     let tmpl = newSTMP source
>     enterM response $ setM contentType ("text/html", Just "utf-8")
>     sendBs $ render . setAttribute "project" project $ tmpl

> getProject :: Handler String
> getProject = do
>     (_:path') <- getM (path % uri % request)
>     let (project, _) = break (== '.') path'
>     return project

> downloadTgz = do
>     enterM response $ setM contentType ("application/x-gzip", Nothing)
>     project <- getProject
>     archive <- liftIO $ prepareTar project
>     sendBs $ G.compress archive

> downloadTbz = do
>     enterM response $ setM contentType ("application/x-bz2", Nothing)
>     project <- getProject
>     archive <- liftIO $ prepareTar project
>     sendBs $ B.compress archive

> root = "/Users/Eric/Projects/E2E/data/06B"

> prepareTar project = do
>     setCurrentDirectory root
>     files   <- liftIO $ recurseDirectories [project]
>     archive <- liftIO $ createTarArchive files
>     return $ writeTarArchive archive

> downloadZip = do
>     liftIO $ setCurrentDirectory root
>     project <- getProject
>     files   <- liftIO $ recurseDirectories [project]
>     archive <- liftIO $ addFilesToArchive [] emptyArchive files
>     enterM response $ setM contentType ("application/zip", Nothing)
>     sendBs $ fromArchive archive

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
