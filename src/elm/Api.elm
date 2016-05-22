effect module Api where { command = MyCmd, subscription = MySub } exposing
  ( onRequest
  , onResponse
  , onError
  , request
  , setUrl
  )

import Task exposing (Task, andThen)
import List
import Process exposing (spawn)
import String
import Maybe
import Dict exposing (Dict)
import Json.Encode exposing (Value)

import Api.LowLevel as Api

--
-- Our basic interface to ths effect manager:
--
onRequest  callback = subscription <| OnRequest  callback
onResponse callback = subscription <| OnResponse callback
onError    callback = subscription <| OnError    callback

request action params mErrFn mResFn = command <| Request action params mErrFn mResFn
setUrl imsUrl = command <| SetUrl imsUrl


--
-- What subscriptions do we have, and how does Sub.map apply to them.
-- basically, how do we wrap the tagging function with another layer.
--
type alias Action = String
type MySub msg
  = OnRequest  ((Action, Api.Params) -> msg)
  | OnResponse ((Action, Api.Params, Api.Result) -> msg)
  | OnError    ((Action, Api.Params, Api.Error) -> msg)

subMap : (a -> b) -> MySub a -> MySub b
subMap func sub = case sub of
    OnRequest  tagger -> OnRequest  (tagger >> func)
    OnResponse tagger -> OnResponse (tagger >> func)
    OnError    tagger -> OnError    (tagger >> func)

--
-- What commands do we have, and how does Cmd.map apply to them
--
type MyCmd msg
  = Request Action Api.Params (Maybe (Api.Error -> msg)) (Maybe (Api.Result -> msg))
  | SetUrl String

cmdMap : (a -> b) -> MyCmd a -> MyCmd b
cmdMap func cmd =
  let
    fn tagger = tagger >> func
  in
    case cmd of
        Request str json onErr onRes ->
            Request str json (Maybe.map fn onErr) (Maybe.map fn onRes)
        SetUrl str -> SetUrl str

--
-- What internal state do we have.
--
type alias ApiRequest msg = (Action, Api.Params, Maybe (Api.Error -> msg), Maybe (Api.Result -> msg))
type alias ApiRequests msg = List (ApiRequest msg)
type alias Model msg =
    { session : Api.Session
    , subs : GroupedSubs msg
    , queuedReqs : ApiRequests msg
    }

init : Task Never (Model msg)
init = Task.succeed
    { session = Api.init
    , subs = initGroupedSubs
    , queuedReqs = []
    }

--
-- entry point for commands and subscriptions from the app.
--
onEffects : Platform.Router msg (SelfMsg msg) -> List (MyCmd msg) -> List (MySub msg) -> Model msg -> Task Never (Model msg)
onEffects router cmds subs model =
  let

    -- Update our model based on input and current one:
    newModel =
      let
        {url,reqs} = compactCommands cmds
        newSess = case url of
            Nothing -> model.session
            Just imsUrl -> Api.setUrl imsUrl model.session
      in
        { model
        | session = newSess
        , subs = compactSubs subs
        , queuedReqs = reqs ++ model.queuedReqs
        }

  in
    case Api.isUrl newModel.session of
        -- No URL provided yet, so just leave requests queued
        False ->
            Task.succeed newModel
        -- URL provided, so perform all of our queued requests
        True ->
            spawn (makeRequests router newModel)
                `Task.andThen` \_ -> Task.succeed { newModel | queuedReqs = [] }

--
-- Make requests. When the results come in, sendToSelf so that
-- we can use the latest state (subs) to inform.
--
makeRequests : Platform.Router msg (SelfMsg msg) -> Model msg -> Task Never ()
makeRequests router {queuedReqs,session} =
  let
    fn ((action,params,_,_) as ap) =
        (IsRequest ap |> Platform.sendToSelf router)
            `Task.andThen` \_ -> Api.request' session action params
            `Task.andThen` (IsResponse ap >> Platform.sendToSelf router)
            `Task.onError` (IsError    ap >> Platform.sendToSelf router)
            `Task.andThen` \_ -> Task.succeed ()
  in
    (List.map (fn >> spawn) queuedReqs |> Task.sequence)
        `Task.andThen` \_ -> Task.succeed ()



--
-- Handle the possible resulting output from makeRequests. This
-- means firing off any subscriptions currently interested at each
-- stage of a request.
--

type SelfMsg msg
    = IsRequest  (ApiRequest msg)
    | IsResponse (ApiRequest msg) (Api.Result, Api.Session -> Api.Session)
    | IsError    (ApiRequest msg) Api.Error

onSelfMsg : Platform.Router msg (SelfMsg msg) -> SelfMsg msg -> Model msg -> Task Never (Model msg)
onSelfMsg router msg ({subs} as state) =
  let

    -- fire some data off to a list of subs and return newState
    sendTo data thisSubs newState =
        Task.sequence (List.map (\sub -> sub data |> Platform.sendToApp router) thisSubs)
            `Task.andThen` \_ -> Task.succeed newState

    -- fire some data off to one maybe sub (if it's not Nothing)
    -- and return newState.
    sendOneMaybe data mThisSub newState =
      let
        doSend = mThisSub
            |> Maybe.map (\fn -> Platform.sendToApp router (fn data))
            |> Maybe.withDefault (Task.succeed ())
      in
        doSend `Task.andThen` \_ -> Task.succeed newState

  in
    case msg of
        -- we've just made a request
        IsRequest  (action,params,onErr,onRes) ->
            sendTo (action,params) subs.onRequest state
        -- we got a response but it was an error
        IsError    (action,params,onErr,onRes) err ->
            sendTo (action,params,err) subs.onError state
                `Task.andThen` sendOneMaybe err onErr
        -- we got a successful response
        IsResponse (action,params,onErr,onRes) (res, sessFn) ->
            sendTo (action,params,res) subs.onResponse { state | session = sessFn state.session }
                `Task.andThen` sendOneMaybe res onRes

--
-- Take a list of commands and separate them out. We can group/remove dupes
-- etc here if needbe.
--

type alias GroupedCmds msg =
    { url : Maybe String
    , reqs : ApiRequests msg
    }

initGroupedCmds = GroupedCmds Nothing []

compactCommands : List (MyCmd msg) -> GroupedCmds msg
compactCommands cmds =
  let
    grouper cmd all = case cmd of
        Request action params onErr onRes ->
            { all | reqs = (action,params,onErr,onRes) :: all.reqs }
        SetUrl url ->
            { all | url = Just url }
  in
    List.foldl grouper initGroupedCmds cmds

--
-- Take a list of subscriptions and separate them out
--

type alias GroupedSubs msg =
    { onRequest  : List ((Action, Api.Params) -> msg)
    , onResponse : List ((Action, Api.Params, Api.Result) -> msg)
    , onError    : List ((Action, Api.Params, Api.Error) -> msg)
    }

initGroupedSubs = GroupedSubs [] [] []

compactSubs : List (MySub msg) -> GroupedSubs msg
compactSubs subs =
  let
    grouper sub all = case sub of
        OnRequest fn  -> { all | onRequest = fn :: all.onRequest }
        OnResponse fn -> { all | onResponse = fn :: all.onResponse }
        OnError fn    -> { all | onError = fn :: all.onError }
  in
    List.foldl grouper initGroupedSubs subs

