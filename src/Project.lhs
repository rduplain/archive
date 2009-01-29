> module Project (getProject, isAllowed, root, withProject) where

> import Control.Monad.Trans            (liftIO)
> import Data.DateTime                  (diffMinutes, getCurrentTime, fromSeconds)
> import Database.HDBC
> import Database.HDBC.PostgreSQL       (connectPostgreSQL)
> import Data.Record.Label
> import Network.Protocol.Http
> import Network.Protocol.Uri
> import Network.Salvia.Handlers.Error  (hError)
> import Network.Salvia.Httpd

> root = "/data/gbt/raw"

> connect = connectPostgreSQL "dbname=vault user=dave"

Extract the project name from the request URI.

> getProject =
>     getM (path % uri % request) >>= return . (fst . break (== '.') . tail)

Determine if the user is permitted access to the requested project.

> isAllowed project = handleSqlError $ do
>     cnn <- connect
>     rst <- quickQuery' cnn "SELECT lastdate FROM projects WHERE name = ?" [toSql project]
>     case rst of
>         [[date]] -> do
>             let date' = fromSeconds . fromSql $ date
>             today <- getCurrentTime
>             return $ today `diffMinutes` date' >= 365*24*60
>         _ -> return False

Conditionally---checking access policy---run a download handler or
return an access denied error.

> withProject         :: (String -> Handler ()) -> Handler ()
> withProject handler = do
>     project <- getProject
>     allowed <- liftIO $ isAllowed project
>     if allowed
>        then handler project
>        else hError Unauthorized
