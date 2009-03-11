#! /usr/bin/env runhaskell

> module Main where

> import Archive
> import Browse
> import CAS
> import Codec.Archive.Tar
> import Codec.Archive.Zip
> import Control.Concurrent                           (forkIO)
> import Control.Concurrent.STM                       (atomically, newTVar)
> import Control.Monad.State                          (get, runStateT)
> import Control.Monad.Trans                          (lift, liftIO)
> import Data.List                                    (isPrefixOf, sort)
> import Data.Maybe                                   (fromJust)
> import Data.Record.Label
> import Handlers
> import Network.Curl                                 (withCurlDo)
> import Network.Protocol.Http                        hiding (hostname)
> import Network.Protocol.Mime
> import Network.Protocol.Uri
> import Network.Salvia.Handlers.Default              (hDefault)
> import Network.Salvia.Handlers.Error
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
> import System.Directory                             (doesDirectoryExist, doesFileExist, getDirectoryContents)
> import System.FilePath                              ((</>), (<.>), takeBaseName)
> import System.IO
> import Text.StringTemplate
> import qualified Codec.Compression.BZip as B
> import qualified Codec.Compression.GZip as G
> import qualified Data.ByteString.Lazy   as L
> import qualified Data.Map as M

> main = withCurlDo $ do
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

> handler         :: SessionHandler CAS ()
> handler session = flip casHandler session $ \session -> hPrefixRouter [
>       ("/browse",    browseHandler session)
>     , ("/download",  downloadHandler False session)
>     , ("/public",    hFileSystem "public")
>     , ("/staged",    stagedHandler session)
>     ] $ hError NotFound

> downloadHandler staging session = hExtensionRouter [
>       (Nothing,     downloadHtml)
>     , (Just "bz2",  downloadTbz staging session)
>     , (Just "html", downloadHtml)
>     , (Just "gz",   downloadTgz staging session)
>     , (Just "zip",  downloadZip staging session)
>     ] $ hError NotFound

> stagedHandler session = do
>     path'  <- getM (path % uri % request)
>     exists <- liftIO $ doesFileExist $ "staged" ++ path'
>     if exists
>        then fileResource $ "staged" ++ path'
>        else downloadHandler True session

> downloadHtml = do
>     source  <- liftIO $ readFile "templates/project_name.html"
>     project <- getProject
>     exists  <- liftIO $ doesDirectoryExist (root </> project)
>     let tmpl = newSTMP source
>     bs <- if exists
>         then return . render . setAttribute "project" project $ tmpl
>         else liftIO $ do
>             projects <- fmap (filter (project `isPrefixOf`)) . getDirectoryContents $ root
>             return . render . setAttribute "projects" (sort . map takeBaseName $ projects) . setAttribute "project" project $ tmpl
>     enterM response $ do
>         setM contentType ("text/html", Just "utf-8")
>         setM contentLength (Just . fromIntegral . L.length $ bs)
>     sendBs bs

Download an archive, optionally keeping a persistent copy of the tarball.

> staged :: String -> String -> (String -> IO L.ByteString) -> Bool -> SessionHandler CAS ()
> staged suffix mime prepare staging session = flip withProject session $ \project _ -> do
>     url   <- getM (uri % request)
>     bytes <- liftIO $ prepare project
>     let path' = "staged" </> project <.> suffix
>     if staging
>        then do
>            liftIO $ forkIO $ L.writeFile path' bytes
>            hRedirect $ lset path ('/':path') url
>        else
>            downloadBytes mime bytes

> fileResource :: ResourceHandler ()
> fileResource file = do
>   let mime = maybe defaultMime id $ (parseURI file >>= mimetype . lget path)
>   safeIO (openBinaryFile file ReadMode)
>     $ \fd -> do
>       bytes <- liftIO $ L.hGetContents fd
>       downloadBytes mime bytes

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
