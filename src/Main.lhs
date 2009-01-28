#! /usr/bin/env runhaskell

> module Main where

> import Codec.Archive.Tar
> import Codec.Archive.Zip
> import Control.Concurrent.STM                      (atomically, newTVar)
> import Control.Monad.State                         (get, runStateT)
> import Control.Monad.Trans                         (lift, liftIO)
> import Data.DateTime
> import Data.List                                   (sort)
> import Data.Maybe                                  (fromJust)
> import Data.Record.Label
> import Database.HDBC
> import Database.HDBC.PostgreSQL
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
> import Numeric                                      (showHex)
> import System.Directory                             (doesDirectoryExist, getCurrentDirectory, setCurrentDirectory)
> import System.FilePath                              ((</>), takeBaseName)
> import System.FilePath.Glob
> import System.IO                                    (Handle, hFlush, hPutStrLn)
> import Text.StringTemplate
> import qualified Codec.Compression.GZip as G
> import qualified Codec.Compression.BZip as B
> import qualified Data.ByteString.Lazy   as L
> import qualified Data.Map as M

> root = "/data/gbt/raw"

> connect = connectPostgreSQL "dbname=vault user=dave"

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
>       ("/download", downloadsHandler)
>     , ("/public",   hFileSystem "public")
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

> getProject :: Handler String
> getProject = do
>     (_:path') <- getM (path % uri % request)
>     let (project, _) = break (== '.') path'
>     return project

> isAllowed :: String -> IO Bool
> isAllowed project = handleSqlError $ do
>     cnn <- connect
>     rst <- quickQuery' cnn "SELECT lastdate FROM projects WHERE name = ?" [toSql project]
>     case rst of
>         [[date]] -> do
>             let date' = fromSeconds . fromSql $ date
>             today <- getCurrentTime
>             return $ today `diffMinutes` date' >= 365*24*60
>         _ -> return False

> withProject         :: (String -> Handler ()) -> Handler ()
> withProject handler = do
>     project <- getProject
>     allowed <- liftIO $ isAllowed project
>     case allowed of
>         True  -> handler project
>         False -> enterM response $ setM status Unauthorized

> downloadTgz = withProject $ \project -> do
>     enterM response $ setM contentType ("application/x-gzip", Nothing)
>     archive <- liftIO $ prepareTar project
>     sendChunked $ G.compress archive

> downloadTbz = withProject $ \project -> do
>     enterM response $ setM contentType ("application/x-bz2", Nothing)
>     archive <- liftIO $ prepareTar project
>     sendChunked $ B.compress archive

> prepareTar project = do
>     dir <- getCurrentDirectory
>     setCurrentDirectory root
>     files   <- liftIO $ recurseDirectories [project]
>     archive <- liftIO $ createTarArchive files
>     setCurrentDirectory dir
>     return $ writeTarArchive archive

> downloadZip = withProject $ \project -> do
>     dir <- liftIO getCurrentDirectory
>     liftIO $ setCurrentDirectory root
>     files   <- liftIO $ recurseDirectories [project]
>     archive <- liftIO $ addFilesToArchive [] emptyArchive files
>     liftIO $ setCurrentDirectory dir
>     enterM response $ setM contentType ("application/zip", Nothing)
>     sendChunked $ fromArchive archive

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
