effect module Store where { command = MyCmd, subscription = MySub } exposing
  ( store
  )

import Debug
import Task exposing (Task, andThen)
import List
import String

import Store.Users as Users

--
-- Expose everything through a record, so we can nest sub-services
-- here and manage everything through one effect manager.
--
store =
    { users = Users.methods (command << UsersCmd) (subscription << UsersSub)
    }

--
-- What subscriptions do we have, and how does Sub.map apply to them.
-- basically, how do we wrap the tagging function with another layer.
--
type MySub msg
  = UsersSub (Users.MySub msg)

subMap : (a -> b) -> MySub a -> MySub b
subMap func sub = case sub of
    UsersSub a -> UsersSub <| Users.subMap func a

--
-- What commands do we have, and how does Cmd.map apply to them
--
type MyCmd msg
  = UsersCmd (Users.MyCmd msg)

cmdMap : (a -> b) -> MyCmd a -> MyCmd b
cmdMap func cmd = case cmd of
    UsersCmd a -> UsersCmd <| Users.cmdMap func a

--
-- What internal store state do we have. This is where our cache would
-- live etc. not exposed outside of this manager.
--
type alias Model =
    { users : Users.Model
    }

init : Task Never Model
init =
    Users.init `andThen` \users ->
        Task.succeed
            { users = users
            }

onEffects : Platform.Router msg () -> List (MyCmd msg) -> List (MySub msg) -> Model -> Task Never Model
onEffects router cmds subs state =
  let

    --
    -- Delegate subscription handling to the relevant module:
    --
    runSub sub model = case sub of
        UsersSub a ->
            Users.runSubscription a model.users
                `andThen` \(users,out) -> Task.succeed ({model | users = users}, out)

    --
    -- Delegate command handling to the relevant module:
    --
    runCmd cmd model = case cmd of
        UsersCmd a ->
            Users.runCommand a model.users
                `andThen` \(users,out) -> Task.succeed ({model | users = users}, out)

    --
    -- Iterate through commands and subscriptions, updating the state as
    -- necessary and sending off any messages back tothe app as required.
    --
    runSubs subs initialState =
      let
        step sub task = task
            `andThen` runSub sub
            `andThen` \(finalState, mMsg) ->
                let succ = Task.succeed finalState
                in case mMsg of
                    Nothing -> succ
                    Just msg -> Platform.sendToApp router msg `andThen` \_ -> succ
      in
        List.foldl step (Task.succeed initialState) subs

    runCmds cmds initialState =
      let
        step cmd task = task
            `andThen` runCmd cmd
            `andThen` \(finalState, mMsg) ->
                let succ = Task.succeed finalState
                in case mMsg of
                    Nothing -> succ
                    Just msg -> Platform.sendToApp router msg `andThen` \_ -> succ
      in
        List.foldl step (Task.succeed initialState) cmds

  in case cmds of
      [] ->
        Task.succeed state
      _ ->
        runCmds cmds state `andThen` runSubs subs


onSelfMsg : Platform.Router msg () -> () -> Model -> Task Never Model
onSelfMsg router lark state =
  Debug.log (toString lark) (Task.succeed state)
