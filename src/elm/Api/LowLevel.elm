module Api.LowLevel exposing (init, request, Session, Error(..), Success) --where

import Http
import Task exposing (Task)
import Regex exposing (regex)
import Json.Encode as Json
import Json.Decode as Decoder exposing (Decoder, (:=))

type alias Session =
    { url : String
    , key : String
    }

type Error
    = ActionError String String
    | ApiError    String String
    | ShapeError  String
    | HttpError   Http.Error

type alias Success
    = Maybe Json.Value

--
-- create a new API session:
--
init : String -> Session
init url = Session (stripTrailingSlash url) ""

--
-- Make an API request given a session, action and input params.
--
request : Session -> String -> List (String, Json.Value) -> Task Error (Success, Session)
request session action data =
  let
    reqUrl = session.url ++ "/api.json.tlx"
    reqBody = Json.encode 0 <| Json.object
        [ "action"     .= Json.string action
        , "inParams"   .= Json.object data
        , "sessionId"  .= Json.string session.key
        , "apiVersion" .= Json.string "1.0"
        ]

    --
    -- We got a valid response! but does it represent an API
    -- or ACTION error? fail if so.
    --
    handleSuccess res =
      let
        status = res.result
        newKey = case (session.key, res.sessionId) of
            ("", val) -> val
            (val, "") -> val
            (_, val) -> val
      in
        if status.action /= "OK" then Task.fail (ActionError status.action res.debug)
        else if status.api /= "OK" then Task.fail (ApiError status.api res.debug)
        else Task.succeed (res.outParams, { session | key = newKey })
    --
    -- Our request failed :( convert it into out ResponseError type
    -- and don't recover
    --
    handleError err = case err of
        Http.UnexpectedPayload str -> Task.fail (ShapeError str)
        other -> Task.fail (HttpError other)
  in
    Http.post decodeResponse reqUrl (Http.string reqBody)
        `Task.onError` handleError
        `Task.andThen` handleSuccess

--
-- Decoding the API response into a basic structure:
--
type alias RawResult =
    { action : String
    , api    : String
    }

type alias RawResponse =
    { result    : RawResult
    , outParams : Maybe Json.Value
    , debug     : String
    , sessionId : String
    }

decodeResponse : Decoder RawResponse
decodeResponse =
  let
    rawResult = Decoder.object2 RawResult
        ("action" := Decoder.string)
        ("api"    := Decoder.string)
    findSessionKey = Decoder.oneOf
        [ ("sessionId" := Decoder.string)
        , Decoder.at ["outParams", "sessionId"] Decoder.string
        , Decoder.succeed ""
        ]
  in
    Decoder.object4 RawResponse
        ("result"    := rawResult)
        ("outParams" := Decoder.maybe Decoder.value)
        ("debug"     := Decoder.oneOf [ Decoder.string, Decoder.succeed "" ])
        ("sessionId" := findSessionKey)

--
-- Helper funcs.
--

stripTrailingSlash : String -> String
stripTrailingSlash = Regex.replace (Regex.AtMost 1) (regex "/$") (\_ -> "")

(.=) : a -> b -> (a,b)
(.=) key val = (key,val)