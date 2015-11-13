module Globals
  (GlobalAction(NoOpGlobal, AddPlayerGlobal, RemovePlayerGlobal,
    StartTournamentGlobal, SetTimeGlobal))
  where
import Time


type GlobalAction =
  NoOpGlobal
  | AddPlayerGlobal String
  | RemovePlayerGlobal String
  | StartTournamentGlobal
  | SetTimeGlobal Time.Time
