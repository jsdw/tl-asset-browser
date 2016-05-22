import Html.App
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Html exposing (..)
--import Store exposing (store)
--import Store.Users as Users
import String

import Api

type alias Model =
    { test : String
    }


type Msg = Text String | Noop

view : Model -> Html Msg
view m =
    div [ ]
        [ text m.test
        ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = (model, Cmd.none)

init : (Model, Cmd Msg)
init =
  let
    cmds = Cmd.batch
        [ Api.setUrl "https://james.chorus.thirdlight.com"
        , Api.request "core.getUserDetails" [] Nothing (Just <| \res -> Text (toString res))
        ]
  in
    (Model "", cmds)

main : Program Never
main =
  Html.App.program
    { init = init
    , update = update
    , view = view
    , subscriptions = \model -> Sub.batch
        [ ]
    }

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