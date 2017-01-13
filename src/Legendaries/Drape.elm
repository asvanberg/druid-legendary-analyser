module Legendaries.Drape exposing (Model, init, parse, bonusHealing)

import Dict exposing (Dict)

import Util.Maybe exposing ((?))

import WarcraftLogs.Models as WCL exposing (Event(..))

type Model = Model (Dict Druid BonusHealing)
type alias Druid = Int
type alias BonusHealing = Int

init : Model
init = Model Dict.empty

parse : WCL.Event -> Model -> Model
parse event (Model druids) =
  case event of
    EncounterStart _ ->
      Model Dict.empty

    Heal {sourceID, ability, amount, overheal, hitType} ->
      if isCrit hitType || isLivingSeed ability then
        let
          -- The total heal is now 210%, 100% from "normal", 100% from critical
          -- bonus and an additional 10% from Drape of Shame
          -- Before it was incorrectly crediting 10% of the total heal to the
          -- cloak
          -- Thanks to Styx of ScrubBusters for finding the bug.
          baseHeal = (amount + overheal) // 210 * 200
          shameHealing = max 0 (amount - baseHeal)
          currentBonus = Dict.get sourceID druids ? 0
        in
          Model <| Dict.insert sourceID (currentBonus + shameHealing) druids
      else
        Model druids

    _ ->
      Model druids

isCrit : Int -> Bool
isCrit = (==) 2

isLivingSeed : WCL.Ability -> Bool
isLivingSeed = ((==) 48503) << .id

bonusHealing : Model -> Druid -> BonusHealing
bonusHealing (Model druids) druid =
  Dict.get druid druids ? 0
