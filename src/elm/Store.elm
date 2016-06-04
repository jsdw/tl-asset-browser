module Store exposing --where
  ( init
  , update
  , Msg(ApiResponse, ApiError)
  )

import Api
import Store.Users as Users exposing (Users)

type alias Store =
    { users : Users
    }

type Msg
    -- public interface:
    = ApiRequest  String Api.Params
    | ApiResponse String Api.Params Api.Result
    | ApiError    String Api.Params Api.Error
    -- private interface:
    | UserMsg     Users.Msg

update : Msg -> Store -> (Store, Cmd Msg)
update msg store =
  let
    cmds userCmd = Cmd.batch
        [ Cmd.map UserMsg userCmd
        ]
    newStore users =
        { store
        | users = users
        }
  in
    case msg of
    -- update individual stores given private messages
    -- triggered from this update or init function:
    UserMsg m ->
        let (users,userCmd) = Users.update m store.users
        in ({ store | users = users}, Cmd.map UserMsg userCmd)
    -- update all stores given api message stuff:
    ApiRequest a p ->
      let
        (users,userCmd) = Users.update (Users.ApiRequest a p) store.users
      in
        (newStore users, cmds userCmd)
    ApiResponse a p r ->
      let
        (users,userCmd) = Users.update (Users.ApiResponse a p r) store.users
      in
        (newStore users, cmds userCmd)
    ApiError a p e ->
      let
        (users,userCmd) = Users.update (Users.ApiError a p e) store.users
      in
        (newStore users, cmds userCmd)


init : (Store, Cmd Msg)
init =
  let
    (users, userCmd) = Users.init
    cmds = Cmd.batch
        [ Cmd.map UserMsg userCmd
        ]
    store =
        { users = users
        }
  in
    (store,cmds)
