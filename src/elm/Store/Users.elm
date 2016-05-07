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
  , User
  )

import Dict as Dict exposing (Dict)
import Task exposing (Task)
import String

type alias User =
    { name : String
    , description : String
    }

--
-- User model (eg cache)
--
type alias Model =
    { userCache : Dict String User
    , currentId : String
    }

init : Task Never Model
init = Task.succeed <|
    { userCache = Dict.empty
    , currentId = ""
    }

--
-- User subscriptions
--
type MySub msg
    = CurrentUser (Maybe User -> msg)
    | IsAuthenticated (Bool -> msg)

subMap : (a -> b) -> MySub a -> MySub b
subMap func sub = case sub of
    CurrentUser tagger ->
        CurrentUser (tagger >> func)
    IsAuthenticated tagger ->
        IsAuthenticated (tagger >> func)

runSubscription : MySub msg -> Model -> Task Never (Model, Maybe msg)
runSubscription sub model = case sub of
    CurrentUser tagger ->
        Task.succeed (model, Just <| tagger Nothing)
    IsAuthenticated tagger ->
        Task.succeed (model, Just <| tagger (String.length model.currentId > 0))

--
-- User commands
--
type MyCmd msg
    = UserDetails String (User -> msg)
    | Login String String
    | Logout

cmdMap : (a -> b) -> MyCmd a -> MyCmd b
cmdMap func cmd = case cmd of
    UserDetails str tagger ->
        UserDetails str (tagger >> func)
    Login name pass ->
        Login name pass
    Logout ->
        Logout

runCommand : MyCmd msg -> Model -> Task Never (Model, Maybe msg)
runCommand cmd model = case cmd of
    UserDetails str tagger ->
        Task.succeed (model, Just <| tagger {name = "James", description = "jam's user"})
    Login name pass ->
        Task.succeed ({ model | currentId = "abcde-12345"}, Nothing)
    Logout ->
        Task.succeed ({ model | currentId = ""}, Nothing)

--
-- Expose our commands and subscriptions through this method,
-- which can be interfaced with an effect manager.
--
methods command subscription =
    { current = \tagger -> subscription (CurrentUser tagger)
    , isAuthenticated = \tagger -> subscription (IsAuthenticated tagger)
    , details = \id tagger -> command (UserDetails id tagger)
    , login = \name pass -> command (Login name pass)
    , logout = command Logout
    }


