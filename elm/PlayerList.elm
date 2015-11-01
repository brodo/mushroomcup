module PlayerList
  (Model, Action, initialModel, update, view,
   focusFilter, focusSelector) where
import Globals
import String
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


-- Model


type alias Model =
  { newPlayer : String
  , isValidPlayerName : Bool
  , showErrorInTextField : Bool
  }


initialModel : Model
initialModel =
  { newPlayer = ""
  , isValidPlayerName = False
  , showErrorInTextField = False }


-- Actions


type Action =
  Global Globals.Action
  | SetNewPlayerName String


--- Update

update : Action -> Model -> GlobalModel -> (Model, GlobalModel)
update action model globalModel =
  case action of
    Global act ->
      case act of
        Globals.Action.NoOp ->
          (model, globalModel)
        Globals.Action.AddPlayer ->
          if model.isValidPlayerName then
            ( { model |
                newPlayer <- ""
              , isValidPlayerName <- False
              , showErrorInTextField <- False
              }
            , { globalModel |
                players <- model.newPlayer :: globalModel.players
              }
            )
          else
            (model, globalModel)
    SetNewPlayerName name ->
      let
        trimmedName = String.trim name
        isValid =
          trimmedName /= "" && not (List.member trimmedName globalModel.players)
      in
        ( { model |
            newPlayer <- name
          , isValidPlayerName <- isValid
          , showErrorInTextField <- True
          }
        , globalModel
        )


-- View

view : Signal.Address Action -> Model -> GlobalModel -> Html
view address model globalModel =
  let
    listItem str =
      li [class "collection-item"] [text str]
    listHeader =
      li [class "collection-header"] [h5 [] [text "Players"]]
    list =
      List.map listItem globalModel.players
    listWithHeaderAndFooter =
      List.append (listHeader :: list) [(addPlayerView address model)]
  in
    ul [ class "collection with-header"] <| listWithHeaderAndFooter


addPlayerView : Signal.Address Action -> Model -> Html
addPlayerView address model =
  let
    setNewPlayerName str =
      Signal.message address (SetNewPlayerName str)
    addPlayer code =
      (if code == 13 then Global.Action.AddPlayer else Global.Action.NoOp)
    textFieldClass =
      if model.showErrorInTextField && not model.isValidPlayerName then
        "invalid"
      else
        "valid"
  in
    li [class "collection-item"]
    [
      div
      [class "input-field "]
      [ i [class "material-icons prefix"] [text "account_circle"]
      , input
        [ id "playername"
        , type' "text"
        , class textFieldClass
        , on "input" targetValue setNewPlayerName
        , onKeyPress address addPlayer
        , value model.newPlayer ]
        []
      , label [for "playername"] [text "Player Name"]
      ]
    ]


-- Focus


focusFilter : Action -> Bool
focusFilter action =
  case action of
    GlobalAction.AddPlayer -> True
    _ -> False


focusSelector : Action -> String
focusSelector _ =
   "#playername"
