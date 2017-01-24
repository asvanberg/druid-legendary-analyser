module Legendaries.Trinket exposing (Model, init, parse, bonusHealing)

import Dict exposing (Dict)
import WarcraftLogs.Models exposing (Event, Event(..))

type alias SourceID = Int
type alias Druid =
  { bonusHealing : Int
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

    Heal {sourceID, amount, overheal} ->
      let
        oldDruid = getDruid druids sourceID
      in
        if (oldDruid.trinketActive) then
          let
            baseHeal = (amount + overheal) // 115 * 100
            bonusHeal = max 0 (amount - baseHeal)
            newDruid =
              { oldDruid
              | bonusHealing = oldDruid.bonusHealing + bonusHeal
              }
          in
            Model <| Dict.insert sourceID newDruid druids
        else
          Model druids

    _ ->
      Model druids

bonusHealing : Model -> SourceID -> Int
bonusHealing (Model druids) sourceID =
  .bonusHealing <| getDruid druids sourceID

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
  { bonusHealing = 0
  , trinketActive = False
  }
