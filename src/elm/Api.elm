effect module Api where { command = MyCmd, subscription = MySub } exposing
  ( store
  )

import Debug
import Task exposing (Task, andThen)
import List
import String
import Json.Encode exposing (Value)

import Api.LowLevel as Api

--
-- What subscriptions do we have, and how does Sub.map apply to them.
-- basically, how do we wrap the tagging function with another layer.
--
type MySub msg
  = OnRequest  ((String, Value) -> msg)
  | OnResponse ((String, Value, Success) -> msg)
  | OnError    ((String, Value, Error) -> msg)

subMap : (a -> b) -> MySub a -> MySub b
subMap func sub = case sub of
    OnRequest  tagger -> OnRequest (tagger >> func)
    OnResponse tagger -> OnResponse (tagger >> func)

--
-- What commands do we have, and how does Cmd.map apply to them
--
type MyCmd msg
  = Request String Value

cmdMap : (a -> b) -> MyCmd a -> MyCmd b
cmdMap func cmd = case cmd of
    Request str json -> Request str json

--
-- What internal state do we have.
--
type alias Model = ()

init : Task Never Model
init = Task.succeed ()

onEffects : Platform.Router msg () -> List (MyCmd msg) -> List (MySub msg) -> Model -> Task Never Model
onEffects router cmds subs state = Debug.log (toString (cmds,subs)) (Task.succeed state)
    --
    -- 1. group matching commands together
    -- 2. fire off any OnRequest subs
    -- 3. map each to an API task whose results get sent to any OnResponse subs.
    --


onSelfMsg : Platform.Router msg () -> () -> Model -> Task Never Model
onSelfMsg router lark state =
    Debug.log (toString lark) (Task.succeed state)



{-

API module provides only low level API functionality in an effect manager, exposes something like

Api.request : String -> Params (Json) -> Cmd msg

Api.onRequest : ((String, ParamJson) -> msg) -> Sub msg
Api.onResponse : ((String, ParamJson, ResultJson) -> msg) -> Sub msg
Api.onChanged : msg -> Sub msg -- like above but less info

Store subscribes to API responses, updating its internal state based on api action, params and resulting data.

Store has sub-modules eg Users, Groups etc whose models are inited with store, but which provide their
own methods to do things, each of which generates an Api Cmd, causing the Store to update. each sub store
receives the response, and so each can choose to ignore or update from it.

main subscribes to API messages. any part can subscribe to Api.changed for general
"store may have changed" notifications.

Store will have to have methods bound to it eg

store.users.get : ID -> (UserDetails, Cmd msg)

where the store will try and pull from its cache and ask to have a command executed if needbe.
each change to a sub store will update its own cache but also the methods it exposes, which will
be partially applied to that cache. so that they dont need it passed in explicitly


-}
