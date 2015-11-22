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



-- Model

type alias GameId = Int

type GameState
  = New
  | Running
  | Finished

type alias Game =
  { places : List (Int, String)
  , state : GameState
  , players : List String
  , id : GameId
  }



newGame : Int -> List String -> Game
newGame id players =
  { places = []
  , state = New
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

maxPlayersPerGame : Int
maxPlayersPerGame =
  4

minPlayersPerGame : Int
minPlayersPerGame =
  2


-- Actions


type Action
  = NoOp
  | StartTournament
  | StartGame Int
  | StopGame Int
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
         , games = (makeGames model.players model.randomSeed)
        }
        , StartTournamentGlobal)
    StartGame id ->
      let
        updater game =
          if game.id == id
            then { game | state = Running }
            else game
      in
        ( { model |
            games = List.map updater model.games
          }
          , NoOpGlobal)
    StopGame id ->
      let
        updater game =
          if game.id == id
            then { game | state = Finished}
            else game
      in
        ( { model |
            games = List.map updater model.games
          }
          , NoOpGlobal)

    Global act ->
      ( updateGlobal act model, NoOpGlobal)




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
  case game.state of
    New ->
      newGameCard address game
    Running ->
      runningGameCard address game
    Finished ->
      finishedGameCard address game

playerListItem : String -> Html
playerListItem player =
  li [class "collection-item"] [text player]


newGameCard : Signal.Address Action -> Game -> Html
newGameCard address game =
  div [ class "card blue-grey lighten-5"]
    [ div [class "card-content black-text" ]
      [ span [class "card-title black-text"]
          [text <| "Game " ++ (toString game.id) ]
      ,  ul [class "collection"] (List.map playerListItem game.players)
      , startGameButton address game
      ]
    ]


runningGameCard : Signal.Address Action -> Game -> Html
runningGameCard address game =
  div [ class  "card  pink darken-2"]
    [ div [class "card-content" ]
      [ span [class "card-title"]
          [text <| "Game " ++ (toString game.id) ]
      ,  ul [class "collection"] (List.map playerListItem game.players)
      , stopGameButton address game
      ]
    ]


finishedGameCard : Signal.Address Action -> Game -> Html
finishedGameCard address game =
  div [ class "card blue-grey"]
    [ div [class "card-content black-text" ]
      [ span [class "card-title black-text"]
          [text <| "Game " ++ (toString game.id) ]
      ,  ul [class "collection"] (List.map playerListItem game.players)
      ]
    ]


startGameButton : Signal.Address Action -> Game -> Html
startGameButton address game =
  div [][
    a [ class ("waves-effect waves-light btn")
      , onClick address (StartGame game.id)
      ] [text "Start Game"]
  ]


stopGameButton : Signal.Address Action -> Game -> Html
stopGameButton address game =
  div [][
    a [ class ("waves-effect waves-light btn")
      , onClick address (StopGame game.id)
      ] [text "Stop Game"]
  ]
