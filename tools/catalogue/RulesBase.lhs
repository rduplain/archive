> module RulesBase (
>     convertToJ2000
>   , guard
>   , keyword
>   , lookup2
>   , makeObservation
>   , remember
>   , require
>   , runRules
>   ) where

> import Control.Monad.Error
> import Control.Monad.Reader
> import Control.Monad.Writer
> import FitsIO
> import Observation
> import System.Posix.Types
> import Utilities
> import WCS

> type Env = [(String, FitsFile, FileOffset)]

> data ObsAtom = ObsS String
>              | ObsD Double
>              deriving Show
> type ObsData = (String, ObsAtom)

> type Rules a = ReaderT Env (ErrorT String (WriterT [ObsData] FitsIO)) a

> runRules       :: Env -> Rules a -> FitsIO (Either String a, [ObsData])
> runRules env f = runWriterT $ runErrorT $ runReaderT f env

> keyword   :: FitsValue a => String -> Rules a
> keyword n = do
>     env <- ask
>     case lookup2 s env of
>       Nothing -> throwError $ "file not found: " ++ s
>       Just f  -> lift $ lift $ lift $ fmap fst (readKey f k)
>   where
>     [s, k] = split '.' n

> lookup2 :: String -> Env -> Maybe FitsFile
> lookup2 x ((k,v,_):kvs)
>   | x == k    = Just v
>   | otherwise = lookup2 x kvs
> lookup2 x _   = Nothing

> require b msg = if b then return () else throwError msg

> class ObsAtomC a where
>   remember :: String -> a -> Rules ()

> instance ObsAtomC String where
>   remember tag atom = tell [(tag, ObsS atom)]

> instance ObsAtomC Double where
>   remember tag atom = tell [(tag, ObsD atom)]

> convertToJ2000              :: String -> Double -> Double -> Rules (Double, Double)
> convertToJ2000 "FK5" ra dec = return (ra, dec)
> convertToJ2000 "FK4" ra dec = do
>   let (ra2000, dec2000) = fk425e (realToFrac ra) (realToFrac dec) 2000
>   return (realToFrac ra2000, realToFrac dec2000)
> convertToJ2000 _     _  _   = throwError "unknown coordinate system"

> makeObservation []     obs = obs
> makeObservation (x:xs) obs = makeObservation xs (makeObsAtom x obs)

> makeObsAtom ("dec",      ObsD x) obs = obs { dec2000  = x }
> makeObsAtom ("observer", ObsS x) obs = obs { observer = x }
> makeObsAtom ("source",   ObsS x) obs = obs { source   = x }
> makeObsAtom ("ra",       ObsD x) obs = obs { ra2000   = x }
> makeObsAtom ("datetime", ObsS x) obs =
>   let date = take 10 x
>       time = take 8 (drop 11 x)
>   in obs { datetime = date ++ ' ':time }
