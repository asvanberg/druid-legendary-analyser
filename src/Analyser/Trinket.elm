module Analyser.Trinket exposing (Model, init, parse, bonusHealing)

import Dict exposing (Dict)
import GenericDict exposing (GenericDict)

import Legendaries
import WarcraftLogs.Models exposing (Event, Event(..))

type alias SourceID = Int
type alias Druid =
  { bonusHealing : GenericDict Legendaries.Source Int
  , trinketActive : Bool
  }

type Model = Model (Dict SourceID Druid)

init : Model
init = Model Dict.empty

parse : Event -> Model -> Model
parse event (Model druids) =
  case event of
    EncounterStart _ ->
      Model Dict.empty

    ApplyBuff {sourceID, ability} ->
      Model <| setTrinketState True sourceID ability druids

    RemoveBuff {sourceID, ability} ->
      Model <| setTrinketState False sourceID ability druids

    Heal {sourceID, ability, amount, overheal} ->
      let
        oldDruid = getDruid druids sourceID
      in
        if (ability.id == 235967) then
          let
            newBonusHealing =
              GenericDict.update Legendaries.Overheal (addBonusHealing amount) oldDruid.bonusHealing

            newDruid =
              { oldDruid | bonusHealing = newBonusHealing }
          in
            Model <| Dict.insert sourceID newDruid druids
        else if (oldDruid.trinketActive) then
          let
            baseHeal = (amount + overheal) // 115 * 100
            bonusHeal = max 0 (amount - baseHeal)
            newDruid =
              { oldDruid
              | bonusHealing = GenericDict.update Legendaries.Increase (addBonusHealing bonusHeal) oldDruid.bonusHealing
              }
          in
            Model <| Dict.insert sourceID newDruid druids
        else
          Model druids

    _ ->
      Model druids

addBonusHealing : Int -> Maybe Int -> Maybe Int
addBonusHealing amount current =
  case current of
    Just currentAmount ->
      Just <| currentAmount + amount

    Nothing ->
      Just amount

bonusHealing : Model -> SourceID -> Legendaries.BonusHealing
bonusHealing (Model druids) sourceID =
  Legendaries.Breakdown <| .bonusHealing <| getDruid druids sourceID

setTrinketState : Bool -> SourceID -> WarcraftLogs.Models.Ability -> Dict SourceID Druid -> Dict SourceID Druid
setTrinketState state sourceID ability druids =
  if (ability.id == 235966) then
    let
      oldDruid = getDruid druids sourceID
      newDruid =
        { oldDruid
        | trinketActive = state
        }
    in
      Dict.insert sourceID newDruid druids
  else
    druids

getDruid : Dict SourceID Druid -> SourceID -> Druid
getDruid druids sourceID =
  Maybe.withDefault newDruid <| Dict.get sourceID druids

newDruid : Druid
newDruid =
  { bonusHealing = GenericDict.empty Legendaries.compareSource
  , trinketActive = False
  }
