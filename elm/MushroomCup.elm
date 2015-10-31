module MushroomCup where
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
  }


initialModel : Model
initialModel =
  { playerList = PlayerList.initialModel
  , games = Games.initialModel
  }

model : Signal Model
model =
  Signal.foldp update initialModel actions.signal


-- Actions


type Action
  = NoOp
  | PlayerList PlayerList.Action
  | Games Games.Action

actions : Signal.Mailbox Action
actions =
  Signal.mailbox NoOp


-- Update


update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model
    PlayerList act ->
      let
        newGames =
          if act == PlayerList.Action.AddPlayer then
            Games.update Games.Action.SetPlayerList model.playerList.players
          else
            model.games
      in
        { model |
          playerList <- PlayerList.update act model.playerList
        , games <- newGames
        }
    Games act ->
      { model |
        games <- Games.update act model.games
      }


-- View


view : Signal.Address Action -> Model -> Html
view address model =
  div [class "row"]
  [
    header
  ,  div [class "col s4"]
    [ (PlayerList.view (Signal.forwardTo address PlayerList) model.playerList) ]
  ,  div [class "col s8"]
    [(Games.view (Signal.forwardTo address Games) model.games)]
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
