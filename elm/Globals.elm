module Globals
  (GlobalAction (NoOpGlobal, AddPlayerGlobal, RemovePlayerGlobal))
  where

type GlobalAction =
  NoOpGlobal
  | AddPlayerGlobal String
  | RemovePlayerGlobal String
