module Store.Api exposing --where
    ( init
    , setUrl
    , getUrl
    , isUrl
    , request
    , mergeSessions
    , Session
    , Params
    , Error(..)
    )

import Http
import String
import Time exposing (Time)
import Task exposing (Task)
import Regex exposing (regex)
import Json.Encode as Json
import Json.Decode as Decoder exposing (Decoder, (:=))

type Session = Session
    { url : String
    , key : String
    , time : Time
    }

type Error
    = ActionError String String
    | ApiError    String String
    | ShapeError  String
    | HttpError   Http.Error
    | NoUrl

type alias Params = List (String, Json.Value)

--
-- create a new API session:
--
init : Session
init = Session { url = "", key = "", time = 0 }

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
-- Make an API request given a session, action and input params. return
-- the response and a new session
--
request : Session -> String -> Params -> Decoder res -> Task Error (res, Session)
request (Session session) action data decoder =
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
        decodeOutput outParams sess = case Decoder.decodeValue decoder outParams of
            Err str -> Task.fail (ShapeError str)
            Ok res -> Task.succeed (res, sess)
      in
        if status.api /= "OK" then Task.fail (ApiError status.api status.debug)
        else if status.action /= "OK" then Task.fail (ActionError status.action status.debug)
        else updateSession res (Session session) `Task.andThen` decodeOutput res.outParams
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
-- Merge sessions
--
mergeSessions : List Session -> Session
mergeSessions sessions =
  let
    (Session blankSess) = init
    merger (Session sess) curr =
        if curr.time > sess.time
        then curr else
            if String.length curr.key > String.length sess.key
            then curr else sess
  in
    Session <| List.foldl merger blankSess sessions

--
-- Take a raw api result and a session and give back
-- a new updated session as a result of it. used
-- to update the session key from the response.
--
updateSession : RawResponse -> Session -> Task x Session
updateSession res (Session session) =
  let
    newKey = case (session.key, res.sessionId) of
        ("", val) -> val
        (val, "") -> val
        (_, val) -> val
    newSess time = Task.succeed <| Session
        { session
        | time = time
        , key = newKey
        }
  in
    Time.now `Task.andThen` newSess

--
-- Decoding the API response into a basic structure:
--
type alias RawResult =
    { action : String
    , api    : String
    , debug  : String
    }

type alias RawResponse =
    { result    : RawResult
    , outParams : Json.Value
    , sessionId : String
    }

decodeResponse : Decoder RawResponse
decodeResponse =
  let
    rawResult = Decoder.object3 RawResult
        ("action" := Decoder.string)
        ("api"    := Decoder.string)
        ("debug"  :=? Decoder.string ?? "")
    getSessionKey = Decoder.oneOf
        [ ("sessionId" := Decoder.string)
        , Decoder.at ["outParams", "sessionId"] Decoder.string
        ]
  in
    Decoder.object3 RawResponse
        ("result"    := rawResult)
        ("outParams" :=? Decoder.value ?? Json.null)
        ("sessionId" :=? getSessionKey ?? "")


--
-- Helper funcs.
--

(:=?) str (decoder, default) =
    Decoder.oneOf [ str := decoder, Decoder.succeed default ]
infixl 5 :=?

(??) a b = (a,b)
infixl 6 ??

stripTrailingSlash : String -> String
stripTrailingSlash = Regex.replace (Regex.AtMost 1) (regex "/$") (\_ -> "")

(.=) : a -> b -> (a,b)
(.=) key val = (key,val)