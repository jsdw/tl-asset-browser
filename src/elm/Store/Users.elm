module Store.Users exposing --where
    ( Users
    , init
    , update
    , Msg(ApiError, ApiResponse)
    )

import Api

type Msg
    = ApiResponse String Api.Params Api.Result
    | ApiError    String Api.Params Api.Error

type alias Users =
    { state   : State
    }

type State = State

update : Msg -> Users -> (Users, Cmd Msg)
update msg users = (users, Cmd.none)

init : (Users, Cmd Msg)
init = (Users State, Cmd.none)