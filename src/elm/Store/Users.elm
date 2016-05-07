module Store.Users exposing -- where
  ( MySub
  , subMap
  , runSubscription
  , MyCmd
  , cmdMap
  , runCommand
  , Model
  , init
  , methods
  )

import Dict as Dict exposing (Dict)
import Task exposing (Task)

type alias User =
    { name : String
    , description : String
    }

--
-- User model (eg cache)
--
type alias Model =
    { userCache : Dict String User
    }

init : Task Never Model
init = Task.succeed (Model Dict.empty)

--
-- User subscriptions
--
type MySub msg
    = CurrentUser (Maybe User -> msg)

subMap : (a -> b) -> MySub a -> MySub b
subMap func sub = case sub of
    CurrentUser tagger ->
        CurrentUser (tagger >> func)

runSubscription : MySub msg -> Model -> Task Never (Model, Maybe msg)
runSubscription sub model = case sub of
    CurrentUser tagger ->
        Task.succeed (model, Just <| tagger Nothing)

--
-- User commands
--
type MyCmd msg
    = UserDetails String (User -> msg)

cmdMap : (a -> b) -> MyCmd a -> MyCmd b
cmdMap func cmd = case cmd of
    UserDetails str tagger ->
        UserDetails str (tagger >> func)

runCommand : MyCmd msg -> Model -> Task Never (Model, Maybe msg)
runCommand cmd model = case cmd of
    UserDetails str tagger ->
        Task.succeed (model, Just <| tagger {name = "James", description = "jam's user"})

--
-- Expose our commands and subscriptions through this method,
-- which can be interfaced with an effect manager.
--
methods command subscription =
    { current = \tagger -> subscription (CurrentUser tagger)
    , details = \id tagger -> command (UserDetails id tagger)
    }


