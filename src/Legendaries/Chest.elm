module Legendaries.Chest exposing (Model, init, parse, bonusHealing)

import Dict exposing (Dict)

import Util.Maybe exposing ((?))

import WarcraftLogs.Models exposing (Event, Event(..))

type Model = Model (Dict Druid BonusHealing)

type alias Druid = Int
type alias BonusHealing = Int

init : Model
init = Model Dict.empty

parse : Event -> Model -> Model
parse event (Model druids) =
  case event of
    EncounterStart _ ->
      Model Dict.empty

    Heal {sourceID, amount, overheal, ability} ->
      if ability.id == 145109 || ability.id == 145110 then
        let
          baseHeal = (amount + overheal) * 2 // 3
          chest = max 0 (amount - baseHeal)
          currentBonus = Dict.get sourceID druids ? 0
        in
          Model <| Dict.insert sourceID (chest + currentBonus) druids
      else
        Model druids

    _ ->
      Model druids

bonusHealing : Model -> Int -> Int
bonusHealing (Model druids) druid =
  Dict.get druid druids ? 0
