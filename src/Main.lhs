#! /usr/bin/env runhaskell

> module Main where

> import Archive
> import Browse
> import Codec.Archive.Tar
> import Codec.Archive.Zip
> import Control.Concurrent                          (forkIO)
> import Control.Concurrent.STM                      (atomically, newTVar)
> import Control.Monad.State                         (get, runStateT)
> import Control.Monad.Trans                         (lift, liftIO)
> import Data.List                                   (sort)
> import Data.Maybe                                  (fromJust)
> import Data.Record.Label
> import Handlers
> import Network.Protocol.Http                        hiding (hostname)
> import Network.Protocol.Uri
> import Network.Salvia.Handlers.Default              (hDefault)
> import Network.Salvia.Handlers.Error                (hError)
> import Network.Salvia.Handlers.ExtensionDispatcher  (hExtensionRouter)
> import Network.Salvia.Handlers.File                 (hFileResource)
> import Network.Salvia.Handlers.FileSystem           (hFileSystem)
> import Network.Salvia.Handlers.PathRouter           (hPrefixRouter)
> import Network.Salvia.Handlers.Redirect             (hRedirect)
> import Network.Salvia.Handlers.Session              (SessionHandler, mkSessions)
> import Network.Salvia.Httpd
> import Network.Socket                               (inet_addr)
> import Numeric                                      (showHex)
> import Project
> import System.Directory                             (doesDirectoryExist, doesFileExist)
> import System.FilePath                              ((</>), (<.>), takeBaseName)
> import System.FilePath.Glob
> import System.IO                                    (Handle, hFlush, hPutStrLn)
> import Text.StringTemplate
> import qualified Codec.Compression.BZip as B
> import qualified Codec.Compression.GZip as G
> import qualified Data.ByteString.Lazy   as L
> import qualified Data.Map as M

> main = do
>     addr <- inet_addr "0.0.0.0"
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
>       ("/browse",   browseHandler)
>     , ("/download", downloadHandler False)
>     , ("/public",   hFileSystem "public")
>     , ("/staged",   stagedHandler)
>     ] $ hError NotFound

> downloadHandler staging = hExtensionRouter [
>       (Nothing,     downloadHtml)
>     , (Just "bz2",  downloadTbz staging)
>     , (Just "html", downloadHtml)
>     , (Just "gz",   downloadTgz staging)
>     , (Just "zip",  downloadZip staging)
>     ] $ hError NotFound

> stagedHandler = do
>     path'  <- getM (path % uri % request)
>     exists <- liftIO $ doesFileExist $ "staged" ++ path'
>     if exists
>        then hFileResource $ "staged" ++ path'
>        else downloadHandler True

> downloadHtml = do
>     source  <- liftIO $ readFile "templates/project_name.html"
>     project <- getProject
>     exists  <- liftIO $ doesDirectoryExist (root </> project)
>     let tmpl = newSTMP source
>     bs <- if exists
>         then return . render . setAttribute "project" project $ tmpl
>         else liftIO $ do
>             ([projects], _) <- globDir [compile $ project ++ "*"] root
>             return . render . setAttribute "projects" (sort . map takeBaseName $ projects) $ tmpl
>     enterM response $ do
>         setM contentType ("text/html", Just "utf-8")
>         setM contentLength (Just . fromIntegral . L.length $ bs)
>     sendBs bs

Download an archive, optionally keeping a persistent copy of the tarball.

> staged :: String -> String -> (String -> IO L.ByteString) -> Bool -> Handler ()
> staged suffix mime prepare staging = withProject $ \project -> do
>     url   <- getM (uri % request)
>     bytes <- liftIO $ prepare project
>     let path' = "staged" </> project <.> suffix
>     if staging
>        then do
>            liftIO $ forkIO $ L.writeFile path' bytes
>            hRedirect $ lset path ('/':path') url
>        else
>            downloadBytes mime bytes

> downloadBytes mime bytes = do
>     enterM response $ setM contentType (mime, Nothing)
>     sendChunked bytes

> downloadTbz = staged ".tar.bz2" "application/x-bzip2" prepareTbz
> downloadTgz = staged ".tar.gz2" "application/x-gzip"  prepareTgz

> prepareTbz = prepareTar B.compress
> prepareTgz = prepareTar G.compress

> prepareTar compress project = do
>     archive <- prepareTar' project
>     return $ compress archive

> downloadZip = staged ".zip" "application/zip" prepareZip

> prepareZip project = do
>     files   <- recurseDirectories [root </> project]
>     archive <- addFilesToArchive [] emptyArchive files
>     return . fromArchive . zipFixPaths $ archive

Construct a tar archive of the specified project.

> prepareTar' project = do
>     files   <- liftIO $ recurseDirectories [root </> project]
>     archive <- liftIO $ createTarArchive files
>     return . writeTarArchive . tarFixPaths $ archive
