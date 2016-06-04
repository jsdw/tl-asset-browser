module Store.Req exposing --where
  ( ApiResult
  , ApiError
  , Req(..)
  , reqMap
  , runReq
  )

import Time exposing (Time)
import Task exposing (Task)
import Json.Decode as Json
import Store.Api as Api

type alias ApiResult =
    { res : Api.Result
    , startTime : Time
    , endTime : Time
    }

type alias ApiError =
    { err : Api.Error
    , startTime : Time
    , endTime : Time
    }

type Req localMsg appMsg
    = ApiRequest String Api.Params (ApiError -> Req localMsg appMsg) (ApiResult -> Req localMsg appMsg)
    | DispatchToApp  appMsg
    | DispatchToSelf localMsg
    | NoReq

reqMap : (a -> b) -> Req a appMsg -> Req b appMsg
reqMap func req = case req of
    ApiRequest str params onErr onRes ->
        ApiRequest str params (onErr >> reqMap func) (onRes >> reqMap func)
    DispatchToSelf localMsg ->
        DispatchToSelf (func localMsg)
    DispatchToApp appMsg ->
        DispatchToApp appMsg
    NoReq ->
        NoReq

runReq : Req localMsg appMsg -> (appMsg -> Task Never ()) -> (localMsg -> Task Never ()) -> Api.Session -> Task Never Api.Session
runReq req toApp toLocal apiSess = case req of
    ApiRequest action params onErr onRes ->
      let
        handleSuccess startTime endTime (rawRes, newSess) =
            let res = ApiResult rawRes startTime endTime
            in runReq (onRes res) toApp toLocal newSess
        handleError startTime endTime err =
            let res = ApiError err startTime endTime
            in runReq (onErr res) toApp toLocal apiSess
      in
        Time.now
            `Task.andThen` \startTime ->
        Task.toResult (Api.request apiSess action params)
            `Task.andThen` \res ->
        Time.now
            `Task.andThen` \endTime -> case res of
                Ok succ -> handleSuccess startTime endTime succ
                Err err -> handleError startTime endTime err
    DispatchToApp appMsg ->
        toApp appMsg `Task.andThen` \_ -> Task.succeed apiSess
    DispatchToSelf localMsg ->
        toLocal localMsg `Task.andThen` \_ -> Task.succeed apiSess
    NoReq ->
        Task.succeed apiSess