module Games (Model, Action, initialModel, update, view) where
import Globals exposing (GlobalModel)
import Html exposing (..)


-- Model


type alias Game =
  { places : List (Int, String)
  , isFinished : Bool
  , isRunning : Bool
  }


type alias Model =
  { games : List Game
  }


initialModel : Model
initialModel =
  { games = []
  }


-- Actions


type Action =
  NoOp

-- Update


update : Action -> Model -> GlobalModel -> Model
update action model globals =
  case action of
    NoOp ->
      model

-- View


view : Signal.Address Action -> Model -> GlobalModel -> Html
view address model globals =
  text <| toString globals
