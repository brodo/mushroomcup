module MushroomCup where
import Globals
import PlayerList
import Tournament


import Time
import Html exposing (..)
import Html.Attributes exposing (..)


-- Model


type alias Model =
  { playerList : PlayerList.Model
  , games : Tournament.Model
  }


initialModel : Model
initialModel =
  { playerList = PlayerList.initialModel
  , games = Tournament.initialModel
  }


ticker : Signal Action
ticker =
  Signal.map (Global << Globals.SetTimeGlobal) (Time.every Time.second)

allSignals : Signal Action
allSignals = Signal.merge actions.signal ticker

model : Signal Model
model =
  Signal.foldp update initialModel allSignals


-- Actions


type Action =
  NoOp
  | PlayerList PlayerList.Action
  | GameList Tournament.Action
  | Global Globals.GlobalAction


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
        (players, globalAction) =
           PlayerList.update act model.playerList
        newModel = updateGlobal globalAction model
      in
        { newModel |
          playerList = players
        }
    GameList act ->
      let
        (games, globalAction) =
          Tournament.update act model.games
        newModel = updateGlobal globalAction model
      in
        { newModel |
          games = games
        }
    Global act ->
      updateGlobal act model


updateGlobal : Globals.GlobalAction -> Model -> Model
updateGlobal action model =
    { model |
      games = Tournament.updateGlobal action model.games
    , playerList = PlayerList.updateGlobal action model.playerList
    }


-- View


view : Signal.Address Action -> Model -> Html
view address model =
  div [class "container"]
  [
    header
    , div [class "row"]
      [ div [class "col s4"]
        [(PlayerList.view (Signal.forwardTo address PlayerList) model.playerList)]
      , div [class "col s8"]
        [(Tournament.view (Signal.forwardTo address GameList) model.games)]
      ]
    , div [class "row"]
      [ div [class "col s12"]
        [ hr [] []
        , text <| toString model
        ]
      ]
  ]


header : Html
header =
  div [class "row"]
  [
    div [class "col s12"]
      [h1 [] [text "Mushroom Cup"]
      ]
  ]


-- Ports and mailboxes


port focus : Signal String
port focus =
  let
    selector action =
      case action of
        PlayerList act ->
          PlayerList.focusSelector act
        _ ->
          ""
    filter action =
      case action of
        PlayerList act ->
          PlayerList.focusFilter act
        _ ->
          False
    filteredActions =
      Signal.filter filter NoOp actions.signal
  in
    Signal.map selector filteredActions



-- Main


main : Signal Html.Html
main = Signal.map (view actions.address) model
