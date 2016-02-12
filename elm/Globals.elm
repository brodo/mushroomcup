module Globals
  (GlobalAction(NoOpGlobal, AddPlayerGlobal, RemovePlayerGlobal,
    StartTournamentGlobal, SetTimeGlobal, RoundFinishedGlobal))
  where
import Time


type GlobalAction =
  NoOpGlobal
  | AddPlayerGlobal String
  | RemovePlayerGlobal String
  | ShowSettings
  | StartTournamentGlobal
  | SetTimeGlobal Time.Time
  | RoundFinishedGlobal
