module Games (Model, Action, initialModel, update, view) where
import Html exposing (..)


-- Model


type alias Game =
  { players : List String
  , places : List (Int, String)
  }

type alias Model =
  { games : List Game
  ,  players : List String
  }


initialModel : Model
initialModel =
  { games = []
  , players = []
  }


-- Actions


type Action =
  NoOp
  | SetPlayerList (List String)

-- Update


update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model
    SetPlayerList list ->
      { model |
        players <- list
      }


-- View


view : Signal.Address Action -> Model -> Html
view address model =
  text <| toString model
