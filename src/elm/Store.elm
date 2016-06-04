effect module Store where { command = MyCmd, subscription = MySub } exposing (store)

import Debug
import Task exposing (Task, andThen)
import List
import String
import Process exposing (spawn)

import Store.Req as Req exposing (runReq, reqMap)
import Store.Users as Users
import Store.Api as Api

--
-- Registry of functions available from this:
--
store =
    { users =
        { current = \tagger -> command <| UserCmd (Users.AskCurrentUser tagger)
        , isAuthenticated = \tagger -> subscription <| UserSub (Users.IsAuthenticated tagger)
        }
    , setUrl = \string -> command <| SetUrl string
    , onChange = \msg -> subscription <| OnChange msg
    }

type MyCmd msg
    = SetUrl String
    | UserCmd (Users.UserCmd msg)

cmdMap : (a -> b) -> MyCmd a -> MyCmd b
cmdMap func cmd = case cmd of
    SetUrl str -> SetUrl str
    UserCmd cmd -> UserCmd <| Users.cmdMap func cmd

type MySub msg
    = OnChange msg
    | UserSub (Users.UserSub msg)

subMap : (a -> b) -> MySub a -> MySub b
subMap func mySub = case mySub of
    OnChange msg -> OnChange (func msg)
    UserSub sub -> UserSub <| Users.subMap func sub

type alias Model msg =
    { apiSession : Api.Session
    , subs : List (MySub msg)
    , users : Users.Model
    }

init : Task Never (Model msg)
init =
    Users.init `andThen` \users ->
        Task.succeed
            { apiSession = Api.init
            , subs = []
            , users = users
            }

-- update subs in model and route commands to onSelfMsg:
onEffects : Platform.Router msg (SelfMsg msg) -> List (MyCmd msg) -> List (MySub msg) -> Model msg -> Task Never (Model msg)
onEffects router cmds subs model =
    (Task.sequence <| List.map (RunCmd >> Platform.sendToSelf router) cmds)
        `endWith` { model | subs = subs }

-- messages that can be sent to self:
type SelfMsg msg
    = FireSubs
    | UpdateApiSess Api.Session
    | RunCmd (MyCmd msg)

-- handle SelfMsg's:
onSelfMsg : Platform.Router msg (SelfMsg msg) -> SelfMsg msg -> Model msg -> Task Never (Model msg)
onSelfMsg router selfMsg model =
  let
    toApp = Platform.sendToApp router
    toSelf = Platform.sendToSelf router
    noop = Task.succeed ()
    runSubs = Task.sequence <| List.map (runSub >> spawn) model.subs

    runSub sub = case sub of
        OnChange msg ->
            toApp msg
        UserSub sub -> case Users.runSub sub model.users of
            Just msg -> toApp msg
            Nothing -> noop

    runCmd cmd = case cmd of
        SetUrl str ->
            Task.succeed { model | apiSession = Api.setUrl str model.apiSession }
        UserCmd cmd ->
            let (users, req) = Users.runCmd cmd model.users
                newModel = { model | users = users}
            in spawn (handleReq UserCmd req newModel) `endWith` newModel

    handleReq tagger req model =
        runSubs
            `Task.andThen` \_ ->
        runReq (reqMap tagger req) toApp (RunCmd >> toSelf) model.apiSession
            `Task.andThen` \newApiSess ->
        toSelf (UpdateApiSess newApiSess)
  in
    case selfMsg of
        UpdateApiSess sess ->
            Task.succeed { model | apiSession = Api.mergeSessions [sess, model.apiSession] }
        FireSubs ->
            runSubs `endWith` model
        RunCmd cmd ->
            runCmd cmd
                `Task.andThen` \model -> (toSelf FireSubs)
                `Task.andThen` \_ -> Task.succeed model


endWith : Task a b -> output -> Task a output
endWith task output = Task.map (\_ -> output) task
