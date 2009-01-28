> module Database (
>     withDB
>   , saveObservation
>   ) where

> import Data.Maybe
> import Database.HaskellDB
> import Database.HaskellDB.HSQL.MySQL
> import Observation hiding (project)
> import Window

> import qualified DBInfo.Detectors     as D
> import qualified DBInfo.Observations  as O
> import qualified DBInfo.Observers     as Q
> import qualified DBInfo.Polarizations as U
> import qualified DBInfo.Projects      as P
> import qualified DBInfo.Receivers     as R
> import qualified DBInfo.Sources       as S
> import qualified DBInfo.Telescopes    as T
> import qualified DBInfo.Windows       as W

> dbOpts = MySQLOptions {
>     server = "localhost"
>   , db     = "vault"
>   , uid    = "powerdave"
>   , pwd    = "Green$Bank"
>   }

> withDB :: (Database -> IO a) -> IO a
> withDB = mysqlConnect dbOpts

> guardedInsert db tbl rec qry = do
>   rst <- query db qry
>   case rst of
>     [] -> insert db tbl rec >> query db qry
>     _  -> return rst

> saveDetector db detector = do
>   let rec = (D.xid <<- Nothing) # (D.name <<- detector)
>   let qry = do tbl <- table D.detectors
>                restrict (tbl!D.name .==. constant detector)
>                project (D.xid << tbl!D.xid)
>   [rst] <- guardedInsert db D.detectors rec qry
>   return (rst!D.xid)

> saveObservation        :: Database -> Observation -> IO ()
> saveObservation db obs = transaction db $ do
>   Just observation_id <- saveObservation' db obs
>   mapM_ (saveWindow db observation_id) $ windows obs

> saveObservation' db obs = do
>   Just project_id  <- saveProject db (proj obs) (observer obs) (telescope obs)
>   Just source_id   <- saveSource db (source obs)
>   let rec = (O.xid        <<- Nothing)      #
>             (O.project_id <<- project_id)   #
>             (O.source_id  <<- source_id)    #
>             (O.scan       <<- scan obs)     #
>             (O.datetime   <<- datetime obs) #
>             (O.exposure   <<- exposure obs) #
>             (O.ra2000     <<- ra2000 obs)   #
>             (O.dec2000    <<- dec2000 obs)  #
>             (O.filesize   <<- filesize obs)
>   let qry = do tbl <- table O.observations
>                restrict (tbl!O.project_id .==. constant project_id .&&.
>                          tbl!O.scan       .==. constant (scan obs))
>                project (O.xid << tbl!O.xid)
>   [rst] <- guardedInsert db O.observations rec qry
>   return (rst!O.xid)

> saveObserver db observer = do
>   let rec = (Q.xid <<- Nothing) # (Q.name <<- observer)
>   let qry = do tbl <- table Q.observers
>                restrict (tbl!Q.name .==. constant observer)
>                project (Q.xid << tbl!Q.xid)
>   [rst] <- guardedInsert db Q.observers rec qry
>   return (rst!Q.xid)

> savePolarization db polarization = do
>   let pol' = if length polarization == 1 then polarization++polarization else polarization
>   let qry = do tbl <- table U.polarizations
>                restrict (tbl!U.name .==. constant pol')
>                project (U.xid << tbl!U.xid)
>   [rst] <- query db qry
>   return (rst!U.xid)

> saveProject db proj observer telescope = do
>   Just observer_id  <- saveObserver db observer
>   Just telescope_id <- saveTelescope db telescope
>   let rec = (P.xid          <<- Nothing)      #
>             (P.observer_id  <<- observer_id)  #
>             (P.telescope_id <<- telescope_id) #
>             (P.name         <<- proj)
>   let qry = do tbl <- table P.projects
>                restrict (tbl!P.name .==. constant proj)
>                project (P.xid << tbl!P.xid)
>   [rst] <- guardedInsert db P.projects rec qry
>   return (rst!P.xid)

> saveReceiver db receiver = do
>   let qry = do tbl <- table R.receivers
>                restrict (tbl!R.name .==. constant receiver)
>                project (R.xid << tbl!R.xid)
>   [rst] <- query db qry
>   return (rst!R.xid)

> saveSource db source = do
>   let rec = (S.xid <<- Nothing) # (S.name <<- source)
>   let qry = do tbl <- table S.sources
>                restrict (tbl!S.name .==. constant source)
>                project (S.xid << tbl!S.xid)
>   [rst] <- guardedInsert db S.sources rec qry
>   return (rst!S.xid)

> saveTelescope db telescope = do
>   let qry = do tbl <- table T.telescopes
>                restrict (tbl!T.name .==. constant telescope)
>                project (T.xid << tbl!T.xid)
>   [rst] <- query db qry
>   return (rst!T.xid)

> saveWindow db observation_id window = do
>   Just detector_id     <- saveDetector db (detector window)
>   Just polarization_id <- savePolarization db (polarization window)
>   Just receiver_id     <- saveReceiver db (receiver window)
>   let rec = (W.xid             <<- Nothing)          #
>             (W.detector_id     <<- detector_id)      #
>             (W.observation_id  <<- observation_id)   #
>             (W.polarization_id <<- polarization_id)  #
>             (W.receiver_id     <<- receiver_id)      #
>             (W.frequency       <<- frequency window) #
>             (W.bandwidth       <<- bandwidth window)
>   insert db W.windows rec
