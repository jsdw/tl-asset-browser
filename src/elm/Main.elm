import Html.App
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Html exposing (..)
import Store exposing (store)
import String

type alias Model =
    { user : String
    , isLoggedIn : Bool
    , username : String
    , password : String
    }

model : Model
model =
    { user = ""
    , isLoggedIn = False
    , username = ""
    , password = ""
    }

type Msg = IsLoggedIn Bool
         | User String
         | Login
         | Logout
         | Username String
         | Password String

view : Model -> Html Msg
view m =
    div [ ]
        [ text <| if String.length m.user > 0 then m.user else "guest"
        , div [] [ text "Name", input [ onInput Username, value m.username ] [] ]
        , div [] [ input [ onInput Password, value m.password ] [] ]
        , button [ onClick Login ] [ text "Login" ]
        , button [ onClick Logout ] [ text "Logout" ]
        ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    noFx a = (a, Cmd.none)
  in case msg of
    IsLoggedIn b ->
        noFx { model | isLoggedIn = b }
    Username str ->
        noFx { model | username = str }
    Password str ->
        noFx { model | password = str }
    User str ->
        noFx { model | user = str }
    Login ->
        (model, store.login model.username model.password)
    Logout ->
        (model, store.logout)

main : Program Never
main =
  Html.App.program
    { init = ( model, Cmd.none )
    , update = update
    , view = view
    , subscriptions = \model -> Sub.batch
        [ store.authState IsLoggedIn
        , store.currentUser User
        ]
    }

