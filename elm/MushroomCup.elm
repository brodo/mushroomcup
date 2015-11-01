module MushroomCup where
import Globals

import PlayerList
import Games


import String
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

-- Model


type alias Model =
  { playerList : PlayerList.Model
  , games : Games.Model
  , globals : Globals.GlobalModel
  }


initialModel : Model
initialModel =
  { playerList = PlayerList.initialModel
  , games = Games.initialModel
  , globals = Globals.initialGlobalModel
  }


model : Signal Model
model =
  Signal.foldp update initialModel actions.signal


-- Actions


type Action =
  Global Globals.Action
  | PlayerList PlayerList.Action
  | Games Games.Action

actions : Signal.Mailbox Action
actions =
  Signal.mailbox Globals.Action.NoOp


-- Update


update : Action -> Model -> Model
update action model =
  case action of
    Globals.Action.NoOp ->
      model
    PlayerList act ->
      let
        (pl, global) = PlayerList.update act model.playerList model.globals
      in
        { model |
          playerList <- pl
        , globals <- global
        }
    Games act ->
      { model |
        games <- Games.update act model.games model.globals
      }



-- View


view : Signal.Address Action -> Model -> Html
view address model =
  div [class "row"]
  [
    header
  ,  div [class "col s4"]
    [ (PlayerList.view (Signal.forwardTo address PlayerList) model.playerList model.globals) ]
  ,  div [class "col s8"]
    [(Games.view (Signal.forwardTo address Games) model.games model.globals)]
  ]


header : Html
header =
  div [class "row"]
  [
    div [class "col s12"]
      [ h1 [] [text "Mushroom Cup"]
      ]
  ]


-- Ports


port focus : Signal String
port focus =
  let
    selector action =
      case action of
        PlayerList act ->
          PlayerList.focusSelector act
        NoOp ->
          ""
    filter action =
      case action of
        PlayerList act ->
          PlayerList.focusFilter act
        NoOp ->
          False
    filteredActions =
      Signal.filter filter NoOp actions.signal
  in
    Signal.map selector filteredActions


-- Main


main : Signal Html.Html
main = Signal.map (view actions.address) model
