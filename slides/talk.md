% From Types to Web Applications
% Alp Mestanogullari
% Haskell eXchange, 2015

# What's wrong with this code?

``` haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Text (Text, toUpper, toLower)
import Web.Scotty

answer :: Bool -> Text -> Text
answer upper texts =
  if upper
    then map toUpper texts
    else map toLower texts

main :: IO ()
main = scotty 3000 $
  post "/endpoint" $ do
    upper <- param "upper"
    ts <- body
    answer upper ts
```

# Or this one?

``` javascript
var express = require('express');
var bodyParser = require('body-parser');
var app = express();
app.use(bodyParser.text());

function answer(upper, ts) {
  if (upper) {
    return ts.toUpperCase();
  } else {
    return ts.toLowerCase();
  }
}

app.post('/endpoint', function (req, res) {
  var upper = req.query.upper && req.query.upper === "true";
  var ts = req.body;
  res.text(answer(upper,ts));
});

app.listen(8080);
```

# Answer

Nothing... unless the outside world needs to talk to those programs!

# Common problem

Once the code is written, there is no way for us to get any type of information about the structure of the webservice or web application!

- What endpoints or pages are available?
- What parameters can an endpoint or page take? Where should those be supplied?
    * Query string parameters: `?q=what%20is%20a%20monad`
    * URL captures: `/hello/:name` will match requests to `/hello/haskellx`, with `name` being `"haskellx"`
    * Request body (in JSON? XML? custom format?)
    * Request headers

This is exactly the kind of information you need when you write functions to query them. If you don't have it, you have to do *everything* manually. Tedious, time-consuming and error-prone.

# Fine, but what can we do about it?

- **Describe** the API
- **Implement** that description to get a runnable webserver
- **Reuse** that description!

We want that description to be written directly in the code, in order to be able to inspect, process and transform it like anything else we manipulate.

# Previous example, revisited with servant

# Describe

``` haskell
import Servant

type API = "endpoint" :> QueryFlag "upper"
                      :> ReqBody '[PlainText] Text
                      :> Post '[PlainText] Text
```

# Implement

``` haskell
type API = "endpoint" :> QueryFlag "upper"
                      :> ReqBody '[PlainText] Text
                      :> Post '[PlainText] Text

api :: Proxy API
api = Proxy

-- our function from the first slide
answer :: Bool -> Text -> Text
answer upper texts =
  if upper
    then map toUpper texts
    else map toLower texts

endpoint :: Server API -- = EitherT ServantErr IO Text
endpoint upper body = return (answer upper body)

main :: IO ()
main = run 8080 (serve api endpoint)
```

# `Proxy` ?

``` haskell
data Proxy a = Proxy
```

Apart from `undefined` and friends, there's only one value of type `Proxy a` for any given `a`.

In other words, when we run the server with `serve api endpoint`, we specifically target an API type and get *servant* to check that the request handlers "match" the API description.

*servant*'s way of asking you which "central description" it should check the implementation against.

# Reuse ?

We now have a description:

``` haskell
type API = "endpoint" :> QueryFlag "upper"
                      :> ReqBody '[PlainText] Text
                      :> Post '[PlainText] Text
```

What do we get ?

# Reuse : haskell functions to query the API (1/2)

``` haskell
type API = "endpoint" :> QueryFlag "upper"
                      :> ReqBody '[PlainText] Text
                      :> Post '[PlainText] Text

api :: Proxy API
api = Proxy

queryEndpoint :: Client API
queryEndpoint = client api (BaseUrl Http "localhost" 8080)
```

`queryEndpoint` is *implemented for us* automatically! Just using our description. It takes the right arguments, serializes them, sends the appropriate HTTP request and finally decodes the response we get from the server.

`Proxy` used again: the client functions are "derived" just by looking at the API type represented by the `Proxy`.

# Reuse : haskell functions to query the API (2/2)

`queryEndpoint` in action.

``` haskell
queryEndpoint False "Hello" -- returns "hello"
queryEndpoint True  "Hello" -- returns "HELLO"
```

# Adding content types

Just add more MIME **types** in the content type list.

``` haskell
type API = "endpoint" :> QueryFlag "upper"
                      :> ReqBody '[PlainText, JSON] Text
                      :> Post '[PlainText, JSON] Text
```

They can all put specific constraints on the types that will be encoded (or decoded) to (or from) their format.

The handlers are agnostic to the content types with which some data they manipulate are encoded. It lets you focus on the business logic.

# Reuse : javascript functions to query the API

``` haskell
type API = "endpoint" :> QueryFlag "upper"
                      :> ReqBody '[PlainText, JSON] Text
                      :> Post '[PlainText, JSON] Text

api :: Proxy API
api = Proxy

writeJS :: FilePath -> IO ()
writeJS fp = writeFile fp (jsForAPI api)

main :: IO ()
main = do
  writeJS "js/api.js"
  run 8080 (serve api endpoint)
```

Automatically "compute" Javascript code to query the API. Write it at server startup. You're guaranteed to get well-behaved functions synchronized with the implementation. `jsForAPI` only needs the `Proxy` to "derive" the Javascript code.

# Serving static files

Use `Raw` for serving static files or more generally "untyped" parts of your application. It is an "escape hatch". Use `:<|>` to separate different (groups of) endpoints and their implementations.

``` haskell
type App = API
      :<|> Raw

app :: Proxy App
app = Proxy

appServer :: Server App
appServer = endpoint
       :<|> serveDirectory "js/"

main :: IO ()
main = run 8080 (serve app appServer)
```

# Deriving a mock server (1/2)

``` haskell
main :: IO ()
main = do
  args <- getArgs
  forkIO runClients
  run 8080 $ case args of
    ["mock"] -> serve app (mock api :<|> serveDirectory "code/talk-examples")
    _        -> serve app appServer
```

# Deriving a mock server (2/2)

Two responses with `mock`:

``` haskell
("\ETB3nhP","\206>\SO,%M,\152\134H")
```

# Reuse: summary

``` haskell
-- we feed servant something whose shape (Server api) depends
-- on the API type. This prevents the DSL to live at the value level,
-- would require dependent types.
serve :: HasServer api => Proxy api -> Server api -> Application

-- we get back something whose shape (Client api) depends
-- on the API type.
client :: HasClient api => Proxy api -> BaseUrl -> Client api

mock :: HasMock api => Proxy api -> Server api
...
```

Every "interpretation" of the API description DSL is implemented using a typeclass. *servant* defines the meaning of `Get`, `ReqBody`, `Capture` and friends in all those contexts. They all rely on the `Proxy` that lets us target the API description type.

# How are `Get`, `ReqBody`, `Capture` and friends defined ?

Most of the time: empty data types!

``` haskell
data Get (contentTypes :: [*]) a
data ReqBody (contentTypes :: [*]) a
data Capture (name :: Symbol) a
```

They all have instances for `HasServer`, `HasClient`, `HasMock`, etc. This is how we make them useful.

# Descriptions as types, types as abstractions (1/2)

Easy to abstract away the boilerplate.

``` haskell
-- managing @a@'s that are
-- indexed by values of type @i@
type Resource (resourceName :: Symbol) i a =
  resourceName :> (
         ReqBody '[JSON] a :> Post '[] () -- submit an 'a'
    :<|> Capture "id" i :> (
                Get '[JSON] a -- view a particular 'a'
           :<|> Delete '[] () -- delete a particular 'a'
      )
  )
```

# Descriptions as types, types as abstractions (2/2)

``` haskell
-- Build the implementation for a 'Resource' out of
-- individual functions
resource :: (a -> IO ()) -- ^ register an 'a'
         -> (i -> IO a)  -- ^ get an 'a' by its identifier
         -> (i -> IO ()) -- ^ delete an 'a' by its identifier
         -> Server (Resource resourcename i a)
resource register getById deleteById =
  register :<|> byId

  where byId i = getById i :<|> deleteById i
```

# Extending the DSL: a combinator to log IP addresses (1/4)

First, define a "dummy", symbolic data type.

``` haskell
data LogIP
```

# Extending the DSL: a combinator to log IP addresses (2/4)

Next, we write an `HasServer` instance.

``` haskell
instance HasServer api => HasServer (LogIP :> api) where
  type ServerT (LogIP :> api) m = ServerT api m

  route _ subserver = \request resp -> do
    print (remoteHost request)
    route (Proxy :: Proxy api) subserver request resp
```

`LogIP` has no effect on handler types, it just prints the host of the client while the request is being routed.

# Extending the DSL: a combinator to log IP addresses (3/4)

We update the API type for our example:

``` haskell
type App = LogIP :> (API :<|> Raw)
```

No need to change the handlers.

# Extending the DSL: a combinator to log IP addresses (4/4)

Output:

``` haskell
127.0.0.1:59767 -- host for first request
127.0.0.1:59767 -- host for second request
("hello","HELLO") -- responses for those requests
```

# Let's now do something interesting

A little web application where users can upload audio files that would then appear on their profile, from which we can play the songs. We'll call it ** Soundskell**. Will demonstrate:

- Authentication
- Supporting JSON (webservice-oriented) and HTML (browser-oriented) interactions
- Extending the type-level DSL with new "combinators" to *augment* the HTTP vocabulary for our needs
- **TODO: Statically checked redirects**
- **TODO: Type-safe links**
- Managing a pool of database connections, used by the request handlers
- API docs generation

# Authentication combinator (1/5)

Keeping it simple.

- A `users` table in a *PostgreSQL* database
- HTTP Basic Authentication for login
- Extend the DSL with an `Auth` combinator to protect auth-restricted parts of an application. You could even reuse it in other projects and make it work with the other interpretations.

# Authentication combinator (2/5)

Users table.

``` sql
create table users
  ( username text primary key
  , pwhash bytea not null
  , joined_on timestamptz not null default now()
  );
```

# Authentication combinator (3/5)

Data types.

``` haskell
data Auth

data User = User { username :: Text, password :: Text }

data AuthProtected handlers = AP
  { checkCreds :: User -> IO Bool
  , onMissingAuthData :: IO Response
  , onCheckFailed :: IO Response
  , protectedHandlers :: handlers
  }

protectWith :: (User -> IO Bool) -> handlers -> AuthProtected handlers
```

# Authentication combinator (4/5)

Server interpretation.

``` haskell
instance HasServer api => HasServer (Auth :> api) where
  type ServerT (Auth :> api) m =
    AuthProtected (User -> ServerT api m)

  -- ... boring details
```

# Authentication combinator (5/5)

Usage:

``` haskell
-- remember:
protectWith :: (User -> IO Bool) -> handlers -> AuthProtected handlers

type SomeAPI = Auth :> Get '[PlainText] Text

server :: Server SomeAPI
server = protectWith (\user -> username == "admin" && password == "admin")
                     (\user -> return $ "Secret data for " <> username user)
```

# File upload (1/2)

We can do the same with file upload. Boring details skipped here but available in the repository for this talk (link in the last slide).

Type declarations:

``` haskell
data Mem -- store uploaded files in memory
data Tmp -- store uploaded files in /tmp

data Files backend

class KnownBackend backend where -- ... skipped
type MultiPartData backend = -- ... skipped
```

Our new construct for the DSL is parametrized by the "backend" used for storing uploaded files.

# File upload (2/2)

Server-side interpretation.

``` haskell
instance (KnownBackend b, HasServer api) => HasServer (Files b :> api) where
  type ServerT (Files b :> api) m =
    MultiPartData b -> ServerT api m
```

Handlers using this new `Files` construct receive the appropriate data as argument.

# Soundskell API

This is the API type for our application.

``` haskell
type API =-- /register
           "register" :> Get '[HTML "register.tpl"] Object
                      -- registration form

      :<|> "register" :> ReqBody '[FormUrlEncoded] User
                      :> Post '[HTML "register_result.tpl"] Object
                      -- registration processing

      :<|> "img" :> Raw   -- serve the image used in the header
      :<|> "songs" :> Raw -- serve the uploaded songs

      :<|> "u" :> Auth :> (
             -- /upload
             "upload" :> Get '[HTML "upload.tpl"] Object
                      -- upload form
        :<|> "upload" :> Files Tmp
                      :> Post '[HTML "upload_result.tpl"] User
                      -- upload processing
        
             -- /u/:user 
        :<|> Capture "user" Username
          :> Get '[HTML "user_profile.tpl"] UserProfile
          -- user profile
      )
```

This includes HTML templates that integrate with servant by being a simple content type.

# Server implementation

``` haskell
server :: Pool Connection -> Server API
server pool = return mempty -- registration form: doesn't need any data
         :<|> register pool -- registration processing
         :<|> serveDirectory "img"
         :<|> serveDirectory "songs"
         :<|> protectWith (\u -> withDB pool $ \conn -> checkUser conn u)
                          (\u -> return mempty -- upload form: no data necessary
                            :<|> uploadSong pool u -- upload processing
                            :<|> (\uname -> getUserProfile pool uname u
                                 -- user profile
                          )
```

# Putting it all together

Our main function is interesting as well.

``` haskell
api :: Proxy API
api = Proxy

main :: IO ()
main = do
  errs <- loadTemplates api "ui/"
  case null errs of
    False -> mapM_ print errs
    True  -> do
      pool <- newPool
      run 8080 (app pool)
```

`loadTemplates` (from *servant-ede*) looks at the template files mentionned in the API type, loads and compiles them. All of this simply inferred from the type.

# Summary

*servant* lets you:

- specify a high-level description of your webservice or application
- this description language can be extended with your own, application-specific constructs
- check implementation of handlers against the description (*servant-server*)
- derive client functions in a few languages (Haskell, Javascript, Ruby) for free (*servant-client*, *servant-js*, *lackey*)
- get API docs with very little work (*servant-docs*)
- get mock servers for free (*servant-mock*)
- (WIP) export to the Swagger API description language, giving *servant* access to a whole new ecosystem

# Questions ?

Useful links:

- slides and code for the examples and soundskell at [http://github.com/alpmestan/haskellx-2015](http://github.com/alpmestan/haskellx-2015)
- website: [http://haskell-servant.github.io/](http://haskell-servant.github.io/) -- links to various useful resources + blog
- tutorial: [http://haskell-servant.github.io/tutorial/](http://haskell-servant.github.io/tutorial/) -- to get started with servant
- code repository: [http://github.com/haskell-servant/servant](http://github.com/haskell-servant/servant) -- with all servant packages
- paper: [http://alpmestan.com/servant/](http://alpmestan.com/servant/) -- explains the type-level DSL approach and the implementation -- accepted at *WGP'15*

















