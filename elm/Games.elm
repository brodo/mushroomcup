module Games (Model, Action(Global), initialModel, update, view) where
import Globals exposing
  (GlobalAction (AddPlayerGlobal, RemovePlayerGlobal, NoOpGlobal))
import Html exposing (..)


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
  }


initialModel : Model
initialModel =
  { games = []
  , players = []
  }

playersPerGame =
  4


-- Actions


type Action
  = NoOp
  | Global GlobalAction

-- Update


update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model
    Global act ->
      case act of
        AddPlayerGlobal player ->
          let
            gameToAddTo 
          in
            { model |
              players <- player :: model.players
            }
        RemovePlayerGlobal player ->
          { model |
             players <- List.filter (\str -> str /= player) model.players
          }
        NoOpGlobal ->
          model


-- View


view : Signal.Address Action -> Model -> Html
view address model =
  text <| toString model
