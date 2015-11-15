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
import ListUtils
import Time


-- Model

type alias Game =
  { places : List (Int, String)
  , isFinished : Bool
  , isRunning : Bool
  , players : List String
  , id : Int
  }

newGame : Int -> List String -> Game
newGame id players =
  { places = []
  , isFinished = False
  , isRunning = False
  , players = players
  , id = id + 1
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


maxPlayersPerGame =
  4


minPlayersPerGame =
  2


-- Actions


type Action
  = NoOp
  | StartTournament
  | StartGame Int
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
         , games <- (makeGames model.players model.randomSeed)
        }
        , StartTournamentGlobal)
    StartGame id ->
      ( model
      , NoOpGlobal)




makeGames : List String -> Random.Seed -> List Game
makeGames players seed =
  let
    randomList = ListUtils.shuffle players seed
    numberOfPlayers = List.length randomList
    canBeDevidedEvenly = (numberOfPlayers % maxPlayersPerGame) == 0
    numberOfCompleteGames = numberOfPlayers // maxPlayersPerGame
    numberOfGames =
      if canBeDevidedEvenly
      then numberOfCompleteGames
      else numberOfCompleteGames + 1
    numberOfPlayersPerGame = numberOfPlayers // numberOfGames
    listOfLists = ListUtils.divideInto randomList numberOfGames
  in
     List.indexedMap newGame listOfLists




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
    SetTimeGlobal time ->
      { model |
        randomSeed <- Random.initialSeed <| truncate time
      }

    _ ->
      model


-- View


view : Signal.Address Action -> Model -> Html
view address model =
  let
    games = List.map (gameCard address) model.games
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

gameCard : Signal.Address Action -> Game -> Html
gameCard address game =
  let
    playerListItem player =
      li [class "collection-item"] [text player]
  in
  div [ class "card blue-grey darken-1" ]
    [ div [class "card-content"]
      [ span [class "card-title"] [text <| "Game " ++ (toString game.id)]
      ,  ul [class "collection"] (List.map playerListItem game.players)
      , startGameButton address game
      ]
    ]

startGameButton : Signal.Address Action -> Game -> Html
startGameButton address game =
  let
    startGameButtonClass =
      if game.isRunning
        then "disabled"
        else ""
  in
    div [][
      a [ class ("waves-effect waves-light btn " ++ startGameButtonClass)
        , onClick address (StartGame game.id)
        ] [if game.isRunning
            then text "Game Finished"
            else text "Start Game"]
    ]
