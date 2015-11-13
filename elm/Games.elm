module Games (Model, Action, initialModel, update, updateGlobal, view) where
import Globals exposing
  (GlobalAction
    (AddPlayerGlobal, RemovePlayerGlobal, NoOpGlobal, StartTournamentGlobal,
    SetTimeGlobal)
  )
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (..)
import Random
import List
import Maybe
import Time


-- Model


type alias Game =
  { places : List (Int, String)
  , isFinished : Bool
  , isRunning : Bool
  , players : List String
  }


type alias Model =
  { games : List Game
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

playersPerGame =
  4


-- Actions


type Action
  = NoOp
  | StartTournament
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
         tournamentStarted <- True
         , players <- shuffleList model.players model.randomSeed
        }
        , StartTournamentGlobal)



-- React to global updates
updateGlobal : Globals.GlobalAction -> Model -> Model
updateGlobal action model =
  case action of
    AddPlayerGlobal player ->
      { model |
        players <- player :: model.players
      }
    RemovePlayerGlobal player ->
      { model |
         players <- List.filter (\str -> str /= player) model.players
      }
    StartTournamentGlobal ->
      { model |
        tournamentStarted <- True
      , players <- shuffleList model.players model.randomSeed
      }
    SetTimeGlobal time ->
      { model |
        randomSeed <- Random.initialSeed <| truncate time
      }

    _ ->
      model




shuffleList : List a -> Random.Seed -> List a
shuffleList list seed =
  let
    listLength = List.length list
    randomListGenerator = Random.list listLength (Random.float 0 10)
    randomList = fst (Random.generate randomListGenerator seed)
    listOfPairs = List.map2 (,) randomList list
    listOfSortedPairs = List.sortBy fst listOfPairs
  in
    List.map snd listOfSortedPairs


-- View


view : Signal.Address Action -> Model -> Html
view address model =
  let
    startButtonClass =
      if List.length model.players > 1 && not model.tournamentStarted
        then ""
        else "disabled"
  in
    div [][
      a [ class ("waves-effect waves-light btn " ++ startButtonClass)
        , onClick address StartTournament
        ] [text "Start Tournament"]
    ]
