module Store.Users exposing --where
  ( Model
  , init
  , UserCmd(AskForDetails)
  , cmdMap
  , runCmd
  , UserSub(..)
  , subMap
  , runSub
  )

import Task exposing (Task)
import Dict exposing (Dict)
import Json.Encode as Json

import Store.Req exposing (..)

-- user model:
type alias Model =
    { detailsCache : Dict String UserDetails
    }

type alias UserDetails = ()
type alias ID = String

init : Task Never Model
init = Task.succeed
    { detailsCache = Dict.empty
    }

-- user cmd:
type UserCmd msg
    -- public API:
    = AskForDetails ID (UserDetails -> msg)
    -- private API:
    | Noop

-- how do taggers get modified by Cmd.map:
cmdMap : (a -> b) -> UserCmd a -> UserCmd b
cmdMap func cmd = case cmd of
    AskForDetails id tagger ->
        AskForDetails id (tagger >> func)
    Noop ->
        Noop


-- run some command. commands can immediately update the model,
-- as well as ask to have future things done.
runCmd : UserCmd msg -> Model -> (Model, Req (UserCmd msg) msg)
runCmd cmd model = case cmd of
    AskForDetails id tagger ->
        ( model
        , ApiRequest "users.getDetails" [("id", Json.string id)]
            (\{err} -> DispatchToSelf <| Noop)
            (\{res} -> DispatchToSelf <| Noop)
        )
    Noop -> (model, NoReq)

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