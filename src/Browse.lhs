> module Browse (browseHandler) where

> import Control.Monad.Reader                  (asks)
> import Control.Monad.Trans                   (liftIO)
> import Data.Fits.FQL                         (keywords, runFQL)
> import Data.Fits.FitsIO                      (runFits)
> import Data.Fits.GBT                         (forEachScan, gbtScans, with)
> import Data.Fits.GBT.ScanLog                 (gbtProject)
> import Data.Record.Label
> import Network.Protocol.Http
> import Network.Protocol.Uri                  (parseQueryParams)
> import Network.Salvia.Handlers.MethodRouter  (hMethodRouter, hPOST)
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

> browseHandler :: Handler ()
> browseHandler = hMethodRouter [
>       (POST, browsePOST)
>     ] $ browseGET

> browseGET = withProject $ \project -> do
>     scans <- liftIO $ scanDescriptions project
>     let bytes = show . htmlprint . formatForm scans $ CElem (Elem "" [] [])
>     enterM response $ do
>         setM contentType ("text/html", Just "utf-8")
>         setM contentLength (Just . fromIntegral . length $ bytes)
>     sendStr bytes

> browsePOST = do
>     bytes <- contents
>     let params = bytes >>= parseQueryParams . L.unpack
>     enterM response $ setM contentType ("text/plain", Nothing)
>     sendStr $ show params

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
