module Settings (Model, initialModel, Action(Global), view, update) where
import Globals exposing
  (GlobalAction(StartTournamentGlobal))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String
import Json.Decode


-- Model


type alias Model =
  { playersPerGame : Int
  , playersGoingToNextRound : Int
  }


initialModel : Model
initialModel =
  { playersPerGame = 4
  , playersGoingToNextRound = 2
  }


-- Actions


type Action =
  NoOp
  | SetPlayersPerGame Int
  | SetPlayersGoingToNextRound Int
  | Global GlobalAction


-- Update

update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model
    SetPlayersPerGame n ->
      let
        ppg = if n >= 2 then n else 2
        newModel = { model | playersPerGame = ppg }
        pnr = (update (SetPlayersGoingToNextRound model.playersGoingToNextRound) newModel).playersGoingToNextRound
      in
        { newModel |
          playersGoingToNextRound = pnr
        }
    SetPlayersGoingToNextRound n ->
      let
        pnr = if n < 1
            then 1
          else if n >= model.playersPerGame
            then model.playersPerGame - 1
          else n
      in
        { model | playersGoingToNextRound = pnr}

    _ ->
      model


-- View


view : Signal.Address Action -> Model -> Html
view address model =
  let
    textFieldClass = "valid"
    numToSetPlayersPerGameMessage num =
      case String.toInt num of
        Ok n ->
          Signal.message address (SetPlayersPerGame n)
        Err _ ->
          Signal.message address NoOp
    numToSetPlayersGoingToNextRoundMessage num =
      case String.toInt num of
        Ok n ->
          Signal.message address (SetPlayersGoingToNextRound n)
        Err _ ->
          Signal.message address NoOp
  in
    div
    [class "settings"]
      [
        h6 [] [text "Round Settings"]
        , div [] [
          div [class "input-field"]
          [  input
            [ id "numberOfPlayers"
            , type' "number"
            , on "change" targetValue numToSetPlayersPerGameMessage
            , value <| toString model.playersPerGame ]
            []
          , label [for "numberOfPlayers"] [text "Players per game"]
          ]
          , div [class "input-field"]
          [ input
            [ id "playersGoingToNextRound"
            , type' "number"
            , on "change" targetValue numToSetPlayersGoingToNextRoundMessage
            , value <| toString model.playersGoingToNextRound ]
            []
          , label [for "numberOfPlayers"] [text "Players advancing to the next round per game"]
          ]
        ]
      ]
