> module Observation (
>     Observation(..)
>   , defaultObservation
>   , proj
>   ) where

> import Window

> data Observation = Observation {
>     project   :: String
>   , observer  :: String
>   , telescope :: String
>   , source    :: String
>   , scan      :: Int
>   , datetime  :: String
>   , exposure  :: Int
>   , ra2000    :: Double
>   , dec2000   :: Double
>   , filesize  :: Int
>   , windows   :: [Window]
>   }

> defaultObservation = Observation {
>     project   = ""
>   , observer  = ""
>   , telescope = ""
>   , source    = ""
>   , scan      = 0
>   , datetime  = ""
>   , exposure  = 0
>   , ra2000    = 0.0
>   , dec2000   = 0.0
>   , filesize  = 0
>   , windows   = []
>   }

> proj = project
