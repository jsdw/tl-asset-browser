module Store.Users exposing --where
  ( Model
  , init
  , UserCmd(AskCurrentUser)
  , cmdMap
  , runCmd
  , UserSub(..)
  , subMap
  , runSub
  )

import Task exposing (Task)
import Dict exposing (Dict)
import Json.Decode as Json
import Json.Encode as Encode

import Store.Req exposing (..)

-- user model:
type alias Model =
    { detailsCache : Dict ID UserDetails
    , currentUser : Maybe UserDetails
    }

type alias UserDetails = ()
type alias ID = String

init : Task Never Model
init = Task.succeed
    { detailsCache = Dict.empty
    , currentUser = Nothing
    }

-- user cmd:
type UserCmd msg

    -- get/cache current user details:
    = AskCurrentUser (UserDetails -> msg)
    | ProcessCurrentUser Json.Value (UserDetails -> msg)

    | Noop

-- how do taggers get modified by Cmd.map:
cmdMap : (a -> b) -> UserCmd a -> UserCmd b
cmdMap func cmd = case cmd of
    AskCurrentUser tagger ->
        AskCurrentUser (tagger >> func)
    ProcessCurrentUser json tagger ->
        ProcessCurrentUser json (tagger >> func)
    Noop ->
        Noop

-- run some command. commands can immediately update the model,
-- as well as ask to have future things done.
runCmd : UserCmd msg -> Model -> (Model, Req (UserCmd msg) msg)
runCmd cmd model = case cmd of
    AskCurrentUser tagger ->
        askCurrentUser tagger model
    ProcessCurrentUser json tagger ->
        processCurrentUser json tagger model
    Noop -> (model, NoReq)

type State = Loading | Loaded

askCurrentUser : (UserDetails -> msg) -> Model -> (Model, Req (UserCmd msg) msg)
askCurrentUser tagger model =
    ( model
    , ApiRequest "core.getCurrentUser" []
        (\{err} -> DispatchToSelf <| Noop)
        (\{res} -> DispatchToSelf <| ProcessCurrentUser res tagger)
    )

processCurrentUser : Json.Value -> (UserDetails -> msg) -> Model -> (Model, Req (UserCmd msg) msg)
processCurrentUser json tagger model = (model, NoReq)
  --let


  --in
  --  ( { model | currentUser = Just currentUser }
  --  , DispatchToApp (tagger currentUser)
  --  )


-- if i do subs as well, they should be as follows.
-- no ability to do tasky bits, just returns a msg or not.
type UserSub msg
    = IsAuthenticated (Bool -> msg)

subMap : (a -> b) -> UserSub a -> UserSub b
subMap func sub = case sub of
    IsAuthenticated tagger ->
        IsAuthenticated (tagger >> func)

runSub : UserSub msg -> Model -> Maybe msg
runSub sub model = case sub of
    IsAuthenticated tagger ->
        Just (tagger True)