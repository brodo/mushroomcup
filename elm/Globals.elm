module Globals
  (GlobalAction(NoOpGlobal, AddPlayerGlobal, RemovePlayerGlobal,
    StartTournamentGlobal))
  where


type GlobalAction =
  NoOpGlobal
  | AddPlayerGlobal String
  | RemovePlayerGlobal String
  | StartTournamentGlobal
