module Globals (GlobalModel, Action (NoOp, AddPlayer), initialGlobalModel) where

type Action =
  NoOp
  | AddPlayer String

type alias GlobalModel =
  { players : List String}

initialGlobalModel : GlobalModel
initialGlobalModel =
  { players = []}
