> module Database (
>     withDB
>   , saveObservation
>   ) where

> import Data.Maybe
> import Database.HDBC
> import Database.HDBC.PostgreSQL
> import Observation hiding (project)
> import Window

> connect = connectPostgreSQL "dbname=vault user=dave"

> withDB   :: (Connection -> IO a) -> IO a
> withDB f = handleSqlError $ do
>     cnn <- connect
>     f cnn

> guardedInsert cnn query insert args = do
>     rst <- quickQuery' cnn query args
>     case rst of
>         [] -> run cnn insert args >> quickQuery' cnn query args
>         _  -> return rst

> saveDetector cnn detector =
>     guardedInsert cnn query insert [toSql detector]
>   where
>     query  = "SELECT id FROM detectors WHERE name = ?"
>     insert = "INSERT INTO detectors(name) VALUES(?)"

> saveObservation cnn obs = do
>     [[observation_id]] <- saveObservation' cnn obs
>     mapM_ (saveWindow cnn observation_id) $ windows obs

> saveObservation' cnn obs = do
>     [[project_id]] <- saveProject cnn (proj obs) (observer obs) (telescope obs)
>     [[source_id]]  <- saveSource cnn (source obs)
>     rst <- quickQuery' cnn query [project_id, toSql (scan obs)]
>     case rst of
>         [] -> do
>             run cnn insert [project_id, source_id, toSql (scan obs), toSql (datetime obs), toSql (exposure obs), toSql (ra2000 obs), toSql (dec2000 obs), toSql (filesize obs)]
>             quickQuery' cnn query [project_id, toSql (scan obs)]
>         _  -> return rst
>   where
>     query  = "SELECT id FROM observations WHERE project_id = ? AND scan = ?"
>     insert = "INSERT INTO observations(project_id, source_id, scan, datetime, exposure, ra2000, dec2000, filesize) VALUES(?, ?, ?, ?, ?, ?, ?, ?)"

> saveObserver cnn observer =
>     guardedInsert cnn query insert [toSql observer]
>   where
>     query  = "SELECT id FROM observers WHERE name = ?"
>     insert = "INSERT INTO observers(name) VALUES(?)"

> savePolarization cnn polarization =
>     quickQuery' cnn query [toSql (take 2 . cycle $ polarization)]
>   where
>     query  = "SELECT id FROM polarizations WHERE name = ?"

> saveProject cnn project observer telescope = do
>     [[observer_id]]  <- saveObserver cnn observer
>     [[telescope_id]] <- saveTelescope cnn telescope
>     rst <- quickQuery' cnn query [toSql project]
>     case rst of
>         [] -> run cnn insert [observer_id, telescope_id, toSql project] >> quickQuery' cnn query [toSql project]
>         _  -> return rst
>   where
>     query  = "SELECT id FROM projects WHERE name = ?"
>     insert = "INSERT INTO projects(observer_id, telescope_id, name) VALUES(?, ?, ?)"

> saveReceiver cnn receiver =
>     quickQuery cnn query [toSql receiver]
>   where
>     query  = "SELECT id FROM receivers WHERE name = ?"

> saveSource cnn source =
>     guardedInsert cnn query insert [toSql source]
>   where
>     query  = "SELECT id FROM sources WHERE name = ?"
>     insert = "INSERT INTO sources(name) VALUES(?)"

> saveTelescope cnn telescope =
>     quickQuery' cnn query [toSql telescope]
>   where
>     query  = "SELECT id FROM telescopes WHERE name = ?"

> saveWindow cnn observation_id window = do
>     [[detector_id]]     <- saveDetector cnn (detector window)
>     [[polarization_id]] <- savePolarization cnn (polarization window)
>     [[receiver_id]]     <- saveReceiver cnn (receiver window)
>     run cnn insert [detector_id, observation_id, polarization_id, receiver_id, toSql (frequency window), toSql (bandwidth window)]
>   where
>     insert = "INSERT INTO windows(detector_id, observation_id, polarization_id, receiver_id, frequency, bandwidth) VALUES(?, ?, ?, ?, ?, ?)"
