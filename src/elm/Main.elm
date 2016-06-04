import Html.App
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Html exposing (..)
import String
import Store exposing (store)

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
update msg model = case msg of
    Noop -> (model, Cmd.none)
    Text str -> Debug.log "update" ({model | test = str}, Cmd.none)

init : (Model, Cmd Msg)
init =
  let
    cmds = Cmd.batch
        [ store.setUrl "https://james.chorus.thirdlight.com"
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
