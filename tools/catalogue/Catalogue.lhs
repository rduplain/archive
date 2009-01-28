> module Main where

> import Control.Exception
> import Control.Monad.Trans
> import Data.List
> import Database
> import Database.HaskellDB.Database (Database)
> import FitsIO
> import Observation
> import Prelude hiding (catch)
> import Rules
> import RulesBase
> import System.Environment
> import System.FilePath
> import System.Posix.Files
> import System.Posix.Types (FileOffset)
> import Utilities
> import Window

> main = do
>   args <- getArgs
>   withDB $ \db ->
>     runFits $ mapM_ (catalogue db) args

> catalogue db scanLog = do
>   readScanLog db scanLog

> readScanLog db fileName = do
>   let (_:_:root) = reverse . splitDirectories $ fileName
>   f <- openFile fileName ReadOnly
>   (project, _) <- readKey f "PROJID"
>   movRelHdu f 1
>   r <- getNumRows f
>   (scan, _) <- readCol f 2 1 1 r 0
>   (filepath, _) <- readCol f 3 1 1 r ""
>   let scans = groupBy fstEq (zip scan filepath)
>   mapM_ (saveObs db project (joinPath . reverse $ root)) scans

> saveObs                    :: Database -> String -> FilePath -> [(Int, FilePath)] -> FitsIO ()
> saveObs db project root ss = do
>   let scan = fst $ head ss
>   env <- mkEnv root ss
>   (rst, xs) <- runRules env rules
>   case rst of
>     Left  err -> liftIO $ showError project scan err
>     Right _   -> do
>       let Just f = lookup2 "IF" env
>       ws <- readWindows f
>       let filesize = fromInteger $ sum [toInteger s | (_, _, s) <- env]
>       let obs = makeObservation xs defaultObservation {
>                     project   = project
>                   , telescope = "GBT"
>                   , exposure  = findExposure ss
>                   , filesize  = filesize
>                   , scan      = scan
>                   , windows   = ws
>                   }
>       liftIO $ (saveObservation db obs >> showStatus project scan ["saved"])
>         `catch` \e -> showError project scan (show e)

> showError project scan error =
>   showStatus project scan ["error", error]

> showStatus project scan xs =
>   putStrLn $ concat $ intersperse "|" $ project:(show scan):xs

> mkEnv           :: FilePath -> [(a, FilePath)] -> FitsIO [(FilePath, FitsFile, FileOffset)]
> mkEnv root scan = mapM (parsePath root) paths >>= return . concat
>   where
>     (_, paths) = unzip scan

> parsePath               :: FilePath -> FilePath -> FitsIO [(FilePath, FitsFile, FileOffset)]
> parsePath root filePath = parsePath' . reverse . splitDirectories $ filePath
>   where
>     parsePath' (_:logName:_) = do
>       let filePath' = joinPath [root, filePath]
>       f <- openFile filePath' ReadOnly
>       s <- getStatus
>       case s of
>         Status 0 -> do
>           sz <- liftIO $ fmap fileSize (getFileStatus filePath')
>           return [(logName, f, sz)]
>         _       -> do
>           clearStatus
>           return []
>     parsePath' _             = return []

> findExposure      :: [(a, String)] -> Int
> findExposure scan = let
>   (_, paths) = unzip scan
>   timestamps = concatMap findTimestamp paths
>   in fromInteger $ maximum timestamps - minimum timestamps

> findTimestamp      :: String -> [Integer]
> findTimestamp scan
>   | ("SCAN":_:"AT":sdate:stime:_) <- words scan = let
>       [hh, mm, ss] = split ':' stime
>       [date, h, m, s] = map read [sdate, hh, mm, ss]
>       in [sum $ zipWith (*) [24*60*60, 60*60, 60, 1] [date, h, m, s]]
>   | otherwise                                   = []

> fstEq (x, _) (y, _)  =  x == y
