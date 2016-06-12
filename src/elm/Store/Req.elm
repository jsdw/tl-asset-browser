module Store.Req exposing --where
  ( ApiResult
  , ApiError
  , Req
  , Params
  , apiRequest
  , toSelf
  , toApp
  , succeed
  , andThen
  , onError
  , toTask
  , none
  )

import Time exposing (Time)
import Task exposing (Task)
import Json.Decode as Json
import Store.Api as Api

type alias ApiResult res =
    { res : res
    , startTime : Time
    , endTime : Time
    }

type alias ApiError =
    { err : Api.Error
    , startTime : Time
    , endTime : Time
    }

type alias Params localMsg appMsg =
    { toApp : (appMsg -> Task Never ())
    , toLocal : (localMsg -> Task Never ())
    , sess : Api.Session
    }

type Req localMsg appMsg err out =
    Req (Params localMsg appMsg -> Task err (out, Params localMsg appMsg))

apiRequest : String -> Api.Params -> Json.Decoder res -> Req localMsg appMsg ApiError (ApiResult res)
apiRequest action apiParams decoder =
    Req <| \params -> Time.now
        `Task.andThen` \startTime ->
            Task.toResult (Api.request params.sess action apiParams decoder)
        `Task.andThen` \res ->
            Time.now
        `Task.andThen` \endTime ->
            case res of
                Ok (res,sess) ->
                    Task.succeed ((ApiResult res startTime endTime), { params | sess = sess})
                Err err ->
                    Task.fail (ApiError err startTime endTime)

toSelf : localMsg -> Req localMsg appMsg Never ()
toSelf msg = Req <| \params -> Task.map (\_ -> ((),params)) (params.toLocal msg)

toApp : appMsg -> Req localMsg appMsg Never ()
toApp msg = Req <| \params -> Task.map (\_ -> ((),params)) (params.toApp msg)

--
-- Standard utility bits (much like Task)
--

succeed : a -> Req localMsg appMsg x a
succeed a = Req (\params -> Task.succeed (a,params))

andThen : Req localMsg appMsg x a -> (a -> Req localMsg appMsg x b) -> Req localMsg appMsg x b
andThen (Req reqfn) fn =
  let newfn (res,params) = let (Req reqfn') = fn res in reqfn' params
  in Req <| \params -> (reqfn params) `Task.andThen` newfn

onError : Req localMsg appMsg a x -> (a -> Req localMsg appMsg b x) -> Req localMsg appMsg b x
onError (Req reqfn) fn =
  let newfn params err = let (Req reqfn') = fn err in reqfn' params
  in Req <| \params -> (reqfn params) `Task.onError` newfn params

toTask : Params localMsg appMsg -> Req localMsg appMsg x a -> Task x a
toTask params (Req reqfn) = Task.map fst (reqfn params)

none : Req localMsg appMsg x ()
none = succeed ()



