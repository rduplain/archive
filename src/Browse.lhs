> module Browse (browseHandler) where

> import Control.Monad.Reader   (asks)
> import Control.Monad.Trans    (liftIO)
> import Data.Fits.FitsIO       (runFits)
> import Data.Fits.FQL          (keywords, runFQL)
> import Data.Fits.GBT          (forEachScan, gbtScans, with)
> import Data.Fits.GBT.ScanLog  (gbtProject)
> import Data.Record.Label
> import Network.Protocol.Http
> import Network.Salvia.Httpd
> import Project                (root, withProject)
> import System.FilePath        ((</>))
> import Text.XML.HaXml         hiding (with)

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
> browseHandler = withProject $ \project -> do
>     scans <- liftIO $ scanDescriptions project
>     let bytes = show . htmlprint . formatScans scans $ CElem (Elem "" [] [])
>     enterM response $ do
>         setM contentType ("text/html", Just "utf-8")
>         setM contentLength (Just . fromIntegral . length $ bytes)
>     sendStr bytes

> formatScans scans = htable [
>       mkElem "thead" [hrow [mkElem "th" [literal c] | c <- "SCAN" : cols]]
>     , mkElem "tbody" [hrow $ (hcol [literal . show $ n]) : [hcol [literal c] | c <- cs] | (n, cs) <- scans]
>     ]

> scanDescriptions         :: String -> IO [(Int, [String])]
> scanDescriptions project = do
>     env <- gbtProject $ root </> project
>     runFits . runFQL env . forEachScan $ do
>         scan    <- asks (fst . head . gbtScans)
>         headers <- with "GO" $ keywords cols
>         return (scan, headers)
