> module CAS where

> import Control.Concurrent.STM
> import Control.Monad.Trans                          (liftIO)
> import Data.Record.Label
> import Network.Curl                                 (curlGetString)
> import Network.Protocol.Http
> import Network.Protocol.Uri
> import Network.Salvia.Handlers.Error                (hError)
> import Network.Salvia.Handlers.Session
> import Network.Salvia.Httpd

Location of our Central Authentication Service.

> cas = "https://my.nrao.edu:8443/cas"

Session type that contains a reference to the current resource
and, optionally, the name of the currently authenticated user.

> data CAS = CAS String (Maybe String) deriving Show

Read the reference to the current resource.

> getResource         :: SessionHandler CAS String
> getResource session = do
>     session' <- liftIO . atomically $ readTVar session
>     let Just (CAS resource _) = payload session'
>     return resource

Read the current username, if any.

> getUsername         :: SessionHandler CAS (Maybe String)
> getUsername session = do
>     session' <- liftIO . atomically $ readTVar session
>     let Just (CAS _ username) = payload session'
>     return username

Record the reference to the current resource.

> putResource                  :: String -> SessionHandler CAS ()
> putResource resource session = liftIO . atomically $ do
>     session' <- readTVar session
>     case payload session' of
>         Just (CAS _ username) ->
>             writeTVar session $ session' { payload = Just $ CAS resource username }
>         Nothing ->
>             writeTVar session $ session' { payload = Just $ CAS resource Nothing }

Record the authenticated user.

> putUsername                  :: Maybe String -> SessionHandler CAS ()
> putUsername username session = liftIO . atomically $ do
>     session' <- readTVar session
>     let Just (CAS resource _) = payload session'
>     writeTVar session $ session' { payload = Just $ CAS resource username }

Save the name of the current resource.

> saveResource         :: SessionHandler CAS ()
> saveResource session = do
>     url <- getM $ path % uri % request
>     flip putResource session $ "http://archive.cv.nrao.edu" ++ url

Top-level handler that is responsible for processing CAS authentication.

> casHandler                 :: SessionHandler CAS () -> SessionHandler CAS ()
> casHandler handler session = do
>     saveResource session
>     params <- fmap queryParams . getM $ uri % request
>     case lookup' Nothing "ticket" params of
>         Nothing     -> handler session
>         Just ticket -> do
>             ticket' <- validate ticket session
>             case ticket' of
>                 Nothing       -> hError Unauthorized
>                 Just username -> do
>                     putUsername (Just username) session
>                     handler session

Call out to the CAS server.

> authenticate         :: SessionHandler CAS ()
> authenticate session = do
>     resource <- getResource session
>     enterM response $ do
>         setM location . parseURI $ cas ++ "/login?service=" ++ encode resource
>         setM status TemporaryRedirect

Ensure the user is authenticated before accessing a protected resource.

> protect :: (String -> SessionHandler CAS ()) -> SessionHandler CAS ()
> protect handler session = do
>     username <- getUsername session
>     case username of
>         Nothing        -> authenticate session
>         Just username' -> handler username' session

Parse the validation response from the CAS server.

> validate                :: String -> SessionHandler CAS (Maybe String)
> validate ticket session = do
>     resource <- getResource session
>     (_, response) <- liftIO . flip curlGetString [] $ cas ++ "/validate?ticket=" ++ encode ticket ++ "&service=" ++ encode resource
>     case lines response of
>         ("no"  : _)            -> return $ Nothing
>         ("yes" : username : _) -> return $ Just username

> lookup'          :: Eq a => Maybe b -> a -> [(a, Maybe b)] -> Maybe b
> lookup' def name = maybe def id . lookup name
