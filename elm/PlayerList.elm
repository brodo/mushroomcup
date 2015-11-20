module PlayerList
  (Model, Action(Global), initialModel, update, updateGlobal, view,
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
  , tournamentStarted : Bool
  }


initialModel : Model
initialModel =
  { newPlayer = ""
  , players = []
  , isValidPlayerName = False
  , showErrorInTextField = False
  , tournamentStarted = True
  }


-- Actions


type Action =
  NoOp
  | AddPlayer
  | SetNewPlayerName String
  | RemovePlayer String
  | Global GlobalAction


--- Update

{-
  This updates the local model and also returns a global action to perform
  on other models by calling the corresponding 'updateGlobal' functions.
-}
update : Action -> Model -> (Model, GlobalAction)
update action model =
  case action of
    NoOp ->
      (model, NoOpGlobal)
    AddPlayer ->
      if model.isValidPlayerName then
        ( { model |
            newPlayer = ""
          , players = model.newPlayer :: model.players
          , isValidPlayerName = False
          , showErrorInTextField = False
          }
          , AddPlayerGlobal model.newPlayer
        )
      else
        (model, NoOpGlobal)
    RemovePlayer player ->
      ( { model |
         players = List.filter (\str -> str /= player) model.players
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
            newPlayer = name
          , isValidPlayerName = isValid
          , showErrorInTextField = True
          }
          , NoOpGlobal
        )
    Global act ->
      (updateGlobal act model, NoOpGlobal)


updateGlobal : GlobalAction -> Model -> Model
updateGlobal action model =
  case action of
    Globals.StartTournamentGlobal ->
      { model |
         tournamentStarted = False
      }
    _ ->
      model


-- View

view : Signal.Address Action -> Model -> Html
view address model =
  let
    listItem str =
      li [class "collection-item"]
        [div []
          [
          text str
          , if model.tournamentStarted
            then
              a [class "secondary-content"
                ,style [("cursor", "pointer")]
                ,onClick address (RemovePlayer str)
                ]
              [i [class "material-icons"] [text "delete"]]
            else
              text ""
          ]
        ]
    listHeader =
      li [class "collection-header"] [h5 [] [text "Players"]]
    list =
      List.map listItem model.players
    listWithHeaderAndFooter =
      if model.tournamentStarted
        then List.append (listHeader :: list) [(addPlayerView address model)]
        else  (listHeader :: list)
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
