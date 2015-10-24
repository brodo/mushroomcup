module Main where
import BaasBox
import Html exposing (..)

main : Signal Html.Html
main = Signal.map (text << .name)  BaasBox.signUpResponses
