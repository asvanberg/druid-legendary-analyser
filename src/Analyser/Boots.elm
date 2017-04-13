module Analyser.Boots exposing (Model, init, parse, bonusHealing)

import Dict exposing (Dict)

import Util.Maybe exposing ((?))

import WarcraftLogs.Models exposing (Event, Event(..))

type Model = Model (Dict Druid BonusHealing)

type alias Druid = Int
type alias BonusHealing = Int

init : Model
init = Model Dict.empty

parse : Event -> Model -> Model
parse event (Model model) =
  case event of
    EncounterStart _ ->
      Model Dict.empty
    Heal {sourceID, hitPoints, maxHitPoints, amount, overheal, ability} ->
      if ability.id == 157982 then
        let
          healthBeforeHeal = hitPoints - amount
          healthBreakpoint = maxHitPoints * 60 // 100
        in
          if (healthBeforeHeal <= healthBreakpoint) then
            let
              baseHeal = (amount + overheal) * 100 // 160
              bonusHealing = max 0 (amount - baseHeal)
              currentBonus = Dict.get sourceID model ? 0
            in
              Model (Dict.insert sourceID (bonusHealing + currentBonus) model)
          else
            Model model
      else
        Model model
    _ ->
      Model model

bonusHealing : Model -> Druid -> Int
bonusHealing (Model model) druid =
  Dict.get druid model ? 0
