module BaasBox (signUp) where

import Json.Encode exposing (..)
import Task

-- Mailboxes

outbox : Signal.Mailbox Json.Encode.Value
outbox = Signal.mailbox Json.Encode.null

inbox : Signal Json.Encode.Value
inbox = baasBoxResonses


-- Ports

port baasBoxCommands : Signal Json.Encode.Value
port baasBoxCommands =
  outbox.signal

port baasBoxResonses : Signal Json.Encode.Value

-- Functions

signUp : String -> String -> Task.Task Json.Encode.Value ()
signUp username password =
  let
    command =
      object
      [ ("command", string "signIn")
      , ("username", string username)
      , ("password", string password)
      ]
  in
    Signal.send outbox.address command
