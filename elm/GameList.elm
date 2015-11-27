module GameList (makeGames, update, view, Action, Model) where
import Random
import ListUtils
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (..)


-- Model


type alias GameId = Int


type ModelState
  = New
  | Running
  | Finished
  | ResultsAdded


type alias Model = List Game


type alias Game =
  { places : List (Int, String)
  , state : ModelState
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


-- Update

update : Action -> Model -> Model
update action model =

  case action of
    StartGame id ->
        let
          updater game =
            if game.id == id
              then { game | state = Running}
              else game
        in
          List.map updater model
    StopGame id ->
      let
        updater game =
          if game.id == id
            then { game | state = Finished}
            else game
      in
        List.map updater model



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


view : Signal.Address Action -> Game -> Html
view address game =
  case game.state of
    New ->
      newGameCard address game
    Running ->
      runningGameCard address game
    Finished ->
      finishedGameCard address game
    ResultsAdded ->
      finishedGameCard address game


playerListItem : String -> Html
playerListItem player =
  li [class "collection-item"] [text player]

playerRankListItem : Int -> String -> Html
playerRankListItem index name =
  li [class "collection-item"]
    [ (text name)
    , span [class "badge"] [text <| toString <| index + 1]
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
      ,  ul [class "collection"] (List.indexedMap playerRankListItem game.players)
      , stopGameButton address game
      ]
    ]


finishedGameCard : Signal.Address Action -> Game -> Html
finishedGameCard address game =
  div [ class "card blue-grey darken-1"]
    [ div [class "card-content" ]
      [ span [class "card-title"]
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
