import Html.App
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Html exposing (..)
import Store exposing (store)
import Store.Users as Users
import String

type alias Model =
    { user : Maybe Users.User
    , isLoggedIn : Bool
    , username : String
    , password : String
    }

model : Model
model =
    { user = Nothing
    , isLoggedIn = False
    , username = ""
    , password = ""
    }

type Msg = IsLoggedIn Bool
         | User (Maybe Users.User)
         | Login
         | Logout
         | Username String
         | Password String

view : Model -> Html Msg
view m =
    div [ ]
        [ text <| if m.isLoggedIn then "yes" else "no"
        , div [] [ text "Name", input [ onInput Username, value m.username ] [] ]
        , div [] [ input [ onInput Password, value m.password ] [] ]
        , button [ onClick Login ] [ text "Login" ]
        , button [ onClick Logout ] [ text "Logout" ]
        ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    noFx a = (a, Cmd.none)
  in case Debug.log "update" msg of
    IsLoggedIn b ->
        noFx { model | isLoggedIn = b }
    Username str ->
        noFx { model | username = str }
    Password str ->
        noFx { model | password = str }
    User str ->
        noFx { model | user = str }
    Login ->
        (model, store.users.login model.username model.password)
    Logout ->
        (model, store.users.logout)

main : Program Never
main =
  Html.App.program
    { init = ( model, Cmd.none )
    , update = update
    , view = view
    , subscriptions = \model -> Sub.batch
        [ store.users.isAuthenticated IsLoggedIn
        , store.users.current User
        ]
    }

