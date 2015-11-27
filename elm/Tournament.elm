module Tournament (Model, Action, initialModel, update, updateGlobal, view) where
import Globals exposing
  (GlobalAction
    (AddPlayerGlobal, RemovePlayerGlobal, NoOpGlobal, StartTournamentGlobal,
    SetTimeGlobal)
  )
import GameList
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (..)
import Random


type alias Model =
  { games : GameList.Model
  , players : List String
  , tournamentStarted : Bool
  , randomSeed: Random.Seed
  }


initialModel : Model
initialModel =
  { games = []
  , players = []
  , tournamentStarted = False
  , randomSeed = Random.initialSeed 4166884
  }



-- Actions


type Action
  = NoOp
  | StartTournament
  | Game GameList.Action
  | Global Globals.GlobalAction


-- Update

{-
  This updates the local model and also returns a global action to perform
  on other models by calling the corresponding 'updateGlobal' functions.
-}
update : Action -> Model -> (Model, GlobalAction)
update action model =
  case action of
    NoOp ->
      (model, NoOpGlobal)
    StartTournament ->
      ( { model |
         tournamentStarted = True
         , games = (GameList.makeGames model.players model.randomSeed)
        }
        , StartTournamentGlobal)
    Game act ->
      ( { model |
          games = GameList.update act model.games
         }
         , NoOpGlobal)
    Global act ->
      ( updateGlobal act model, NoOpGlobal)


-- React to global updates
updateGlobal : Globals.GlobalAction -> Model -> Model
updateGlobal action model =
  case action of
    AddPlayerGlobal player ->
      { model |
        players = player :: model.players
      }
    RemovePlayerGlobal player ->
      { model |
         players = List.filter (\str -> str /= player) model.players
      }
    SetTimeGlobal time ->
      { model |
        randomSeed = Random.initialSeed <| truncate time
      }

    _ ->
      model


-- View


view : Signal.Address Action -> Model -> Html
view address model =
  let
    gameAddress = (Signal.forwardTo address Game)
    games = List.map (GameList.view gameAddress) model.games
    button = startTournamentButton address model
  in
    div [] (button :: games)


startTournamentButton : Signal.Address Action -> Model -> Html
startTournamentButton address model =
  let
    startTournamentButtonClass =
      if List.length model.players > 1 && not model.tournamentStarted
        then ""
        else "disabled"
  in
    div [][
      a [ class ("waves-effect waves-light btn " ++ startTournamentButtonClass)
        , onClick address StartTournament
        ] [if model.tournamentStarted
            then text "Tournament Started!"
            else text "Start Tournament"]
    ]
