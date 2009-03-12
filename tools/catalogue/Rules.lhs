> module Rules (rules) where
> import RulesBase

> import Control.Monad.Trans

> rules = do

>   source <- keyword "GO.OBJECT"
>   -- liftIO $ putStrLn $ "source: " ++ source
>   require (length source >= 2) "missing source name"
>   remember "source" (source :: String)

>   observer <- keyword "GO.OBSERVER"
>   -- liftIO $ putStrLn $ "observer: " ++ observer
>   require (length observer >= 3) "missing observer name"
>   remember "observer" (observer :: String)

>   datetime <- keyword "GO.DATE-OBS"
>   -- liftIO $ putStrLn $ "datetime: " ++ datetime
>   require ("2001-01-01" <= datetime) $ "invalid date: " ++ datetime
>   remember "datetime" datetime

>   raDec


> raDec = do
>   radesys <- keyword "GO.RADESYS"
>   raDeg   <- keyword "GO.RA"
>   decDeg  <- keyword "GO.DEC"
>   (ra2000, dec2000) <- convertToJ2000 radesys raDeg decDeg
>   require (0 <= ra2000 && ra2000 < 360) "right ascension not in range"
>   require (-180 <= dec2000 && dec2000 <= 180) "declination not in range"

>   require (dec2000 >= -51) "not visible from Green Bank"
>   remember "ra" ra2000
>   remember "dec" dec2000
