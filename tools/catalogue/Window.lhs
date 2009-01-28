> module Window (
>     Window(..)
>   , readWindows
>   ) where

> import Control.Monad.Trans
> import Data.List
> import FitsIO

> data Window = Window {
>     detector     :: String
>   , polarization :: String
>   , receiver     :: String
>   , frequency    :: Double
>   , bandwidth    :: Double
>   }

> readWindows f = do
>   movRelHdu f 1
>   r <- getNumRows f
>   [cDetector, cPolarization, cReceiver, cFrequency, cBandwidth] <-
>     mapM (getColNum f False) ["BACKEND", "POLARIZE", "RECEIVER", "CENTER_SKY", "BANDWDTH"]
>   [(dDetector, _), (dPolarization, _), (dReceiver, _)] <-
>     mapM (\c -> readCol f c 1 1 r "") [cDetector, cPolarization, cReceiver]
>   [(dFrequency, _), (dBandwidth, _)] <-
>     mapM (\c -> readCol f c 1 1 r 0.0) [cFrequency, cBandwidth]
>   return $ zipWith5 Window dDetector dPolarization dReceiver dFrequency dBandwidth
