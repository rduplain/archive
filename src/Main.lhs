#! /usr/bin/env runhaskell

> module Main where

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

Abstract out the physical location of the project.

> tarFixPaths archive@(TarArchive { archiveEntries = entries }) =
>     archive { archiveEntries = map fixEntry entries }
>   where
>     fixEntry entry@(TarEntry { entryHeader = header }) =
>         entry { entryHeader = fixHeader header }
>     fixHeader header@(TarHeader { tarFileName = path }) =
>         header { tarFileName = drop (length root) path }

Abstract out the physical location of the project.

> zipFixPaths archive@(Archive { zEntries = entries }) =
>     archive { zEntries = map fixEntry entries }
>   where
>     fixEntry entry@(Entry { eRelativePath = path }) =
>         entry { eRelativePath = drop (length root + 1) path }

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
