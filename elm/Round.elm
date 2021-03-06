module Round
  (makeGames, newRound, update, updateGlobal, view, Action, Model)
  where
import Random
import ListUtils
import List.Extra
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (..)
import Settings
import Random
import Globals exposing (GlobalAction(StartTournamentGlobal, RoundFinishedGlobal, NoOpGlobal))



-- Model


type alias GameId = Int
type alias PlayerName  = String


type GameState
  = New
  | Running
  | Finished
  | ResultsAdded


type RoundState
  = RunningRound
  | FinishedRound
  | SetupRound


type alias Model =
  { games : List Game
  , id : Int
  , state : RoundState
  , settings : Settings.Model
  , players : List String
  , randomSeed : Random.Seed
  }


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

newRound : List Game -> List String -> Int -> Random.Seed -> Model
newRound games players id seed=
  { games = games
  , id = id
  , state = SetupRound
  , settings = Settings.initialModel
  , players = players
  , randomSeed = seed
  }


-- Actions


type Action
  = StartGame GameId
  | StopGame GameId
  | MoveUp PlayerName
  | MoveDown PlayerName
  | PlacementsReady GameId
  | StartRound
  | Global Globals.GlobalAction
  | Settings Settings.Action


-- Update


update : Action -> Model -> (Model, Globals.GlobalAction)
update action model =
  case action of
    Global act ->
      (updateGlobal act model, NoOpGlobal)
    StartGame id ->
      let
        updater game =
          if game.id == id then
              { game |
                state = Running
              , places = List.indexedMap (,) game.players
              }
            else
              game
      in
        (
          { model |
            games = List.map updater model.games
          }
          , NoOpGlobal
        )
    StopGame id ->
      let
        updater game =
          if game.id == id
            then { game | state = Finished}
            else game
      in
        (
          { model |
            games = List.map updater model.games
          },
          NoOpGlobal
        )
    PlacementsReady id ->
      let
        updater game =
          if game.id == id
            then { game | state = ResultsAdded}
            else game
        newGames = List.map updater model.games
        newState = if List.all (\g -> g.state == ResultsAdded) newGames
          then FinishedRound
          else RunningRound
      in
        (
          { model |
            games = newGames
          , state = newState
          }
          , if newState == FinishedRound then RoundFinishedGlobal else NoOpGlobal
        )
    MoveUp name ->
      let
        updatePlaces place (i,n) =
          if i == (place - 1) then
            (i+1, n)
          else if n == name then
            (i-1, n)
          else
            (i,n)
      in
        (placeUpdater name model updatePlaces, NoOpGlobal)
    MoveDown name ->
      let
        updatePlaces place (i, n) =
          if i == (place + 1) then
            (i-1, n)
          else if n == name then
            (i+1, n)
          else
            (i,n)
      in
        (placeUpdater name model updatePlaces, NoOpGlobal)
    StartRound ->
      (
        { model |
          games = makeGames model
        , state = RunningRound
        }
        , NoOpGlobal
      )
    Settings act ->
      (
        { model |
          settings = Settings.update act model.settings
        }
        , NoOpGlobal
      )


-- React to global updates
updateGlobal : Globals.GlobalAction -> Model -> Model
updateGlobal action model =
  case action of
    _ ->
      model



placeUpdater : String -> Model -> (Int -> (Int, String) -> (Int, String)) -> Model
placeUpdater name model updater =
  let
    game = gameForPlayer name model
    (place,_) =
      Maybe.withDefault (0, "") <|
        List.Extra.find (\(_,n) -> n == name) game.places
    newGame = { game | places = List.map (updater place) game.places }
  in
    { model |
      games = List.map (\g-> if g.id == game.id then newGame else g) model.games
    }


gameForPlayer : String -> Model -> Game
gameForPlayer playerName model =
  let
    game = List.head <| List.filter (\g -> List.member playerName g.players) model.games
  in
    Maybe.withDefault (newGame 0 []) game


makeGames : Model -> List Game
makeGames model =
  let
    randomList = ListUtils.shuffle model.players model.randomSeed
    numberOfPlayers = List.length randomList
    canBeDevidedEvenly = (numberOfPlayers % model.settings.playersPerGame) == 0
    numberOfCompleteModels = numberOfPlayers // model.settings.playersPerGame
    numberOfModels =
      if canBeDevidedEvenly
      then numberOfCompleteModels
      else numberOfCompleteModels + 1
    numberOfPlayersPerModel = numberOfPlayers // numberOfModels
    listOfLists = ListUtils.divideInto randomList numberOfModels
  in
     List.indexedMap newGame listOfLists


-- View


view : Signal.Address Action -> Model -> Html
view address round =
  let
    settingsAddress = (Signal.forwardTo address Settings)
    gameviews = List.map (gameView address) round.games
    settingsView = Settings.view settingsAddress round.settings
    setupView = div []
      [ settingsView
      , a
        [ class "waves-effect waves-light btn"
        , onClick address StartRound
        ]
        [ text "Start round" ]
      ]


    finisedStr = if round.state == FinishedRound
      then " (finished)"
      else " (running)"
  in
    div [class "round"]
    [ h5 [] [text ("Round " ++ (toString round.id) ++ finisedStr)]
    , if round.state == SetupRound
      then setupView
      else div [class "games"] gameviews
    , hr [][]
    ]




gameView : Signal.Address Action -> Game -> Html
gameView address game =
  case game.state of
    New ->
      newGameCard address game
    Running ->
      runningGameCard address game
    Finished ->
      finishedGameCard address game
    ResultsAdded ->
      resultsGameCard address game


playerListItem : String -> Html
playerListItem player =
  li [class "collection-item"] [text player]

playerRankListItem : Int -> String -> Html
playerRankListItem index name  =
  let
    rank = index + 1
  in
    li [class "collection-item"]
      [ span [class "chip"] [text <| toString rank]
      , span [class "oneRemLeft"] [
          text name
        ]
      ]


finishedPlayerRankListItem : Signal.Address Action -> Int -> Int -> String -> Html
finishedPlayerRankListItem address playerCount index name  =
  let
    rank = index + 1
    isLast = rank == playerCount
    isFirst = index == 0
    upButton =
      a [ class "waves-effect waves-light btn fourRemLeft"
        , onClick address (MoveUp name)]
        [text  "↑"]
    downButton =
      a [ class "waves-effect waves-light btn oneRemLeft"
        , onClick address (MoveDown name)]
        [text  "↓"]
    buttons =
      if isLast then
        [upButton]
      else if isFirst then
        [downButton]
      else
        [upButton, downButton]
  in
    li [class "collection-item"]
      [ span [class "chip"] [text <| toString rank]
      , span [class "oneRemLeft"] [text name]
      , span [class "right"] buttons
      ]


newGameCard : Signal.Address Action -> Game -> Html
newGameCard address game =
  div [ class "card blue-grey lighten-5 animated slideInRight"]
    [ div [class "card-content black-text" ]
      [ span [class "card-title black-text"]
          [text <| "Game " ++ (toString game.id) ]
      ,  ul [class "collection"] (List.map playerListItem game.players)
      , startGameButton address game
      ]
    ]


runningGameCard : Signal.Address Action -> Game -> Html
runningGameCard address game =
  div [ class  "card  blue-grey lighten-3"]
    [ div [class "card-content black-text" ]
      [ span [class "card-title black-text"]
          [text <| "Game " ++ (toString game.id) ]
      ,  ul [class "collection"]
            (List.map playerListItem  game.players)
      , stopGameButton address game
      ]
    ]


finishedGameCard : Signal.Address Action -> Game -> Html
finishedGameCard address game =
  let
    playerCount = List.length game.players
    playerRankListItemWithCount = finishedPlayerRankListItem address playerCount
    sortedPlaces = List.sortBy (\(i,_) -> i) game.places
    players = List.map (\(_,n) -> n) sortedPlaces
  in
    div  [ class "card blue-grey darken-1"]
      [ div [class "card-content" ]
        [ span [class "card-title"]
            [text <| "Game " ++ (toString game.id) ]
        ,  ul [class "collection"]
              (List.indexedMap playerRankListItemWithCount players)
        , placesReadyButton address game
        ]
      ]


resultsGameCard : Signal.Address Action -> Game -> Html
resultsGameCard address game =
  let
    playerCount = List.length game.players
    playerRankListItemWithCount = finishedPlayerRankListItem address playerCount
    sortedPlaces = List.sortBy (\(i,_) -> i) game.places
    players = List.map (\(_,n) -> n) sortedPlaces
  in
    div  [ class "card blue-grey darken-1"]
      [ div [class "card-content" ]
        [ span [class "card-title"]
            [text <| "Game " ++ (toString game.id) ]
        ,  ul [class "collection"]
              (List.indexedMap playerRankListItem players)
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


placesReadyButton : Signal.Address Action -> Game -> Html
placesReadyButton address game =
  div [][
    a [ class ("waves-effect waves-light btn")
      , onClick address (PlacementsReady game.id)
      ] [text "Ok"]
  ]
