module Api.LowLevel exposing --where
    ( init
    , setUrl
    , getUrl
    , isUrl
    , request
    , request'
    , Session
    , Params
    , Error(..)
    , Result
    )

import Http
import String
import Task exposing (Task)
import Regex exposing (regex)
import Json.Encode as Json
import Json.Decode as Decoder exposing (Decoder, (:=))

type Session = Session
    { url : String
    , key : String
    }

type Error
    = ActionError String String
    | ApiError    String String
    | ShapeError  String
    | HttpError   Http.Error
    | NoUrl

type alias Result
    = Maybe Json.Value

type alias Params = List (String, Json.Value)

--
-- create a new API session:
--
init : Session
init = Session { url = "", key = "" }

--
-- Public interface to api session:
--

setUrl : String -> Session -> Session
setUrl url (Session s) = Session { s | url = stripTrailingSlash url }

getUrl : Session -> String
getUrl (Session s) = s.url

isUrl : Session -> Bool
isUrl (Session s) = String.length s.url > 0

--
-- As below but return an updated session rather than a function
-- to perform the updating.
--
request : Session -> String -> Params -> Task Error (Result, Session)
request session action data =
    request' session action data `Task.andThen` \(res,fn) -> Task.succeed (res,fn session)

--
-- Make an API request given a session, action and input params. return
-- the response and a function to update a session based on it, or fail with the error
--
request' : Session -> String -> Params -> Task Error (Result, Session -> Session)
request' (Session session) action data =
  let

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
      in
        if status.action /= "OK" then Task.fail (ActionError status.action res.debug)
        else if status.api /= "OK" then Task.fail (ApiError status.api res.debug)
        else Task.succeed (res.outParams, updateSession res)
    --
    -- Our request failed :( convert it into out ResponseError type
    -- and don't recover
    --
    handleError err = case err of
        Http.UnexpectedPayload str -> Task.fail (ShapeError str)
        other -> Task.fail (HttpError other)

    --
    -- Make a request given a URL to make it to:
    --
    makeRequest url = Http.post decodeResponse url (Http.string reqBody)
        `Task.onError` handleError
        `Task.andThen` handleSuccess

  in
    case session.url of
        ""  -> Task.fail NoUrl
        url -> makeRequest (url ++ "/api.json.tlx")

--
-- Take a raw api result and a session and give back
-- a new updated session as a result of it. used
-- to update the session key from the response.
--
updateSession : RawResponse -> Session -> Session
updateSession res (Session session) =
  let
    newKey = case (session.key, res.sessionId) of
        ("", val) -> val
        (val, "") -> val
        (_, val) -> val
  in
    Session { session | key = newKey }

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