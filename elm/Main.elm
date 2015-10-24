module Main where
import BaasBox
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)

-- Action
type Action = NoOp
  | SignUp String String


-- Model
type alias Model = {
    user : String
  , isLoggedIn : Bool
}

initialModel : Model
initialModel = {user = "", isLoggedIn = False}


-- Update

update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model
    SignUp username password ->
      let
        _ = BaasBox.signUp username password
      in
        model


-- View

view : Signal.Address Action -> Model -> Html
view address model =
  div [] [
    input [
            on "input" targetValue (Signal.message address)
          , value model.username
          ]
  ]


main : Signal Html
main = Signal.map initialModel  BaasBox.signUpResponses
