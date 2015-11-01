module Globals ( GlobalAction (NoOpGlobal, AddPlayerGlobal)) where

-- Action


type GlobalAction =
  NoOpGlobal
  | AddPlayerGlobal String
