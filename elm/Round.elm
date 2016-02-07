module Round (makeGames, newRound, update, view, Action, Model) where
import Random
import ListUtils
import List.Extra
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (..)


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

type alias Model =
  { games : List Game
  , id : Int
  , state : RoundState
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

newRound : List Game -> Int -> Model
newRound games id=
  { games = games
  , id = id
  , state = RunningRound
  }

maxPlayersPerModel : Int
maxPlayersPerModel =
  4

minPlayersPerModel : Int
minPlayersPerModel =
  2

-- Actions

type Action
  = StartGame GameId
  | StopGame GameId
  | MoveUp PlayerName
  | MoveDown PlayerName
  | PlacementsReady GameId

-- Update

update : Action -> Model -> Model
update action model =
  case action of
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
          { model |
            games = List.map updater model.games
          }
    StopGame id ->
      let
        updater game =
          if game.id == id
            then { game | state = Finished}
            else game
      in
        { model |
          games = List.map updater model.games
        }
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
        { model |
          games = newGames
        , state = newState
        }
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
        placeUpdater name model updatePlaces
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
        placeUpdater name model updatePlaces



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

makeGames : List String -> Random.Seed -> List Game
makeGames players seed =
  let
    randomList = ListUtils.shuffle players seed
    numberOfPlayers = List.length randomList
    canBeDevidedEvenly = (numberOfPlayers % maxPlayersPerModel) == 0
    numberOfCompleteModels = numberOfPlayers // maxPlayersPerModel
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
    gameviews = List.map (gameView address) round.games
    finisedStr = if round.state == FinishedRound
      then " (finished)"
      else " (running)"
  in
    div [class "round"]
    [
      h5 [] [text ("Round " ++ (toString round.id) ++ finisedStr)]
    ,  div [class "games"] gameviews
    ,  hr [][]
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
