> module Browse (browseHandler) where

> import Archive
> import CAS
> import Codec.Archive.Tar
> import Codec.Compression.BZip
> import Control.Monad.Reader                  (asks)
> import Control.Monad.Trans                   (liftIO)
> import Data.Fits.FQL                         (keywords, runFQL)
> import Data.Fits.FitsIO                      (runFits)
> import Data.Fits.GBT                         (forEachScan, gbtScans, with)
> import Data.Fits.GBT.ScanLog                 (gbtProject)
> import Data.List                             (nub)
> import Data.Record.Label
> import Handlers
> import Network.Protocol.Http
> import Network.Protocol.Uri                  (parseQueryParams)
> import Network.Salvia.Handlers.MethodRouter  (hMethodRouter, hPOST)
> import Network.Salvia.Handlers.Session
> import Network.Salvia.Httpd
> import Project                               (root, withProject)
> import System.FilePath                       ((</>))
> import Text.XML.HaXml                        hiding (with)
> import qualified Data.ByteString.Lazy.Char8 as L

> cols = [ "OBJECT"
>        , "OBSID"
>        , "PROCNAME"
>        , "PROCTYPE"
>        , "PROCSEQN"
>        , "PROCSIZE"
>        , "OBSTYPE"
>        , "SWSTATE"
>        , "SWTCHSIG"
>        , "LASTON"
>        , "LASTOFF"
>        ]

> browseHandler         :: SessionHandler CAS ()
> browseHandler session = hMethodRouter [
>       (POST, browsePOST session)
>     ] $ browseGET session

> browsePOST session = flip withProject session $ \project _ -> do
>     scans <- collectScans
>     paths <- liftIO $ collectFiles project scans
>     bytes <- liftIO $ prepareDownload paths
>     enterM response $ do
>         setM contentType ("application/x-bzip2", Nothing)
>         setM (header "Content-Disposition") $ "attachment; filename=\"" ++ project ++ ".tar.bz2\""
>     sendChunked bytes

> collectScans :: Handler [Int]
> collectScans = do
>     bytes <- contents
>     case bytes >>= parseQueryParams . L.unpack of
>         Just params -> return [read v | (k, Just v) <- params, k == "scan"]
>         Nothing     -> return []

> collectFiles :: String -> [Int] -> IO [FilePath]
> collectFiles project scans = do
>     env <- gbtProject $ root </> project
>     let paths = [root </> project </> instrument </> file | (scan, files) <- gbtScans env, scan `elem` scans, (instrument, file) <- files]
>     return $ (root </> project </> "ScanLog.fits") : nub paths

> prepareDownload files = do
>     archive <- createTarArchive files
>     return . compress . writeTarArchive . tarFixPaths $ archive

> browseGET session = flip withProject session $ \project _ -> do
>     scans <- liftIO $ scanDescriptions project
>     let bytes = show . htmlprint . formatForm scans $ CElem (Elem "" [] [])
>     enterM response $ do
>         setM contentType ("text/html", Just "utf-8")
>         setM contentLength (Just . fromIntegral . length $ bytes)
>     sendStr bytes

> formatForm scans = mkElemAttr "form" [("method", literal "POST")] [
>       formatScans scans
>     , mkElemAttr "input" [("type", literal "submit"), ("value", literal "Download")] []
>     ]
      
> formatScans scans = htable [
>       mkElem "thead" [hrow [mkElem "th" [literal c] | c <- "" : "SCAN" : cols]]
>     , mkElem "tbody" [formatRow scan | scan <- scans]
>     ]

> formatRow (n, cs) = hrow $
>       hcol [mkElemAttr "input" [("type", literal "checkbox"), ("name", literal "scan"), ("value", literal . show $ n)] []]
>     : hcol [literal . show $ n]
>     : [hcol [literal c] | c <- cs]

> scanDescriptions         :: String -> IO [(Int, [String])]
> scanDescriptions project = do
>     env <- gbtProject $ root </> project
>     runFits . runFQL env . forEachScan $ do
>         scan    <- asks (fst . head . gbtScans)
>         headers <- with "GO" $ keywords cols
>         return (scan, headers)
