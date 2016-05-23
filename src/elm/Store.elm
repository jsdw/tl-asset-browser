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
    = ApiResponse String Api.Params Api.Result
    | String Api.Params Api.Error
    -- private interface:
    | UserMsg     Users.Msg

update : Msg -> Store -> (Store, Cmd Msg)
update msg store = case msg of
    -- update individual stores given private messages:
    UserMsg m ->
        let (users,userCmd) = Users.update m store.users
        in ({ store | users = users}, Cmd.map UserMsg userCmd)
    -- update all stores given api messages:
    ApiResponse a p r ->
      let
        (users,userCmd) = Users.update (Users.ApiResponse a p r) store.users
        cmds = Cmd.batch
            [ Cmd.map UserMsg userCmd
            ]
        newStore =
            { users = users
            }
      in
        (newStore, cmds)
    ApiError a p e ->
      let
        (users,userCmd) = Users.update (Users.ApiError a p e) store.users
        cmds = Cmd.batch
            [ Cmd.map UserMsg userCmd
            ]
        newStore =
            { users = users
            }
      in
        (newStore, cmds)


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
