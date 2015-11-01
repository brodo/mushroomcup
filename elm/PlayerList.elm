module PlayerList
  (Model, Action, initialModel, update, view,
   focusFilter, focusSelector) where
import Globals exposing
  (GlobalAction(AddPlayerGlobal, NoOpGlobal, RemovePlayerGlobal))
import String
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


-- Model


type alias Model =
  { newPlayer : String
  , players : List String
  , isValidPlayerName : Bool
  , showErrorInTextField : Bool
  }


initialModel : Model
initialModel =
  { newPlayer = ""
  , players = []
  , isValidPlayerName = False
  , showErrorInTextField = False }


-- Actions


type Action =
  NoOp
  | AddPlayer
  | SetNewPlayerName String
  | RemovePlayer String


--- Update


update : Action -> Model -> (Model, GlobalAction)
update action model =
  case action of
    NoOp ->
      (model, NoOpGlobal)
    AddPlayer ->
      if model.isValidPlayerName then
        ( { model |
            newPlayer <- ""
          , players <- model.newPlayer :: model.players
          , isValidPlayerName <- False
          , showErrorInTextField <- False
          }
          , AddPlayerGlobal model.newPlayer
        )
      else
        (model, NoOpGlobal)
    RemovePlayer player ->
      ( { model |
         players <- List.filter (\str -> str /= player) model.players
        }
        , RemovePlayerGlobal player
      )
    SetNewPlayerName name ->
      let
        trimmedName = String.trim name
        isValid =
          trimmedName /= "" && not (List.member trimmedName model.players)
      in
        ( { model |
            newPlayer <- name
          , isValidPlayerName <- isValid
          , showErrorInTextField <- True
          }
          , NoOpGlobal
        )


-- View

view : Signal.Address Action -> Model -> Html
view address model =
  let
    listItem str =
      li [class "collection-item"]
        [div []
          [text str
          ,a [class "secondary-content"
              ,style [("cursor", "pointer")]
              ,onClick address (RemovePlayer str)
              ]
            [i [class "material-icons"] [text "delete"]]
          ]
        ]
    listHeader =
      li [class "collection-header"] [h5 [] [text "Players"]]
    list =
      List.map listItem model.players
    listWithHeaderAndFooter =
      List.append (listHeader :: list) [(addPlayerView address model)]
  in
    ul [class "collection with-header"] <| listWithHeaderAndFooter


addPlayerView : Signal.Address Action -> Model -> Html
addPlayerView address model =
  let
    setNewPlayerName str =
      Signal.message address (SetNewPlayerName str)
    addPlayer code =
      (if code == 13 then AddPlayer else NoOp)
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
    AddPlayer ->
      True
    _ ->
      False

focusSelector : Action -> String
focusSelector _ =
   "#playername"
