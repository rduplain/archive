#! /usr/bin/env runhaskell

> import Database.HaskellDB
> import Database.HaskellDB.HSQL.MySQL
> import DBInfo.Receivers

> dbOpts = MySQLOptions {
>     server = "localhost"
>   , db     = "gbtarchive"
>   , uid    = "gbtarchive"
>   , pwd    = "Green$Bank"
>   }

> withDB = mysqlConnect dbOpts

> testQuery q db = do
>   putStrLn "Query:"
>   print q
>   result <- query db q
>   putStrLn "Result:"
>   mapM_ print result

> selectAllReceivers = table receivers

> main = withDB $ testQuery selectAllReceivers
