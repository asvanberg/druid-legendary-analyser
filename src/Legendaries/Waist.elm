module Legendaries.Waist exposing (Model, init, parse, bonusHealing)

import Dict exposing (Dict)
import Time exposing (Time, millisecond)
import Util.Maybe exposing ((?))

import WarcraftLogs.Models exposing (Event, Event(..))

type Model = Model (Dict CharacterID Druid)

type alias CharacterID = Int
type alias Druid =
  { bonusHealing : Int
  , lookingForDefaultBloom : DefaultBloom
  }
type DefaultBloom = Yes Time | No
type alias BonusHealing = Int

timeForDefaultBloom : Time
timeForDefaultBloom = 100 * millisecond

init : Model
init = Model Dict.empty

parse : Event -> Model -> Model
parse event (Model druids) =
  Model <| case event of
    EncounterStart _ ->
      Dict.empty

    RefreshBuff event ->
      let
        (Model newDruids) = parse (RemoveBuff event) (Model druids)
      in
        newDruids

    RemoveBuff {timestamp, sourceID, ability} ->
      if ability.id == 33763 then
        let
          druid =
            getDruid druids sourceID
          newDruid =
            { druid | lookingForDefaultBloom = Yes timestamp }
        in
          Dict.insert sourceID newDruid druids
      else
        druids

    Heal {timestamp, sourceID, amount, overheal, ability} ->
      if ability.id == 33778 then
        let
          druid =
            getDruid druids sourceID
          baseHeal =
            (amount + overheal) // 3
          bloomIncrease =
            max 0 (amount - baseHeal)
          belt =
            case druid.lookingForDefaultBloom of
              Yes refreshed ->
                if timestamp - refreshed <= timeForDefaultBloom then
                  bloomIncrease
                else
                  amount

              No ->
                amount
          newDruid =
            { druid
            | bonusHealing = druid.bonusHealing + belt
            , lookingForDefaultBloom = No
            }
        in
          Dict.insert sourceID newDruid druids
      else
        druids

    _ ->
      druids

bonusHealing : Model -> Int -> Int
bonusHealing (Model druids) druid =
  (Maybe.map .bonusHealing <| Dict.get druid druids) ? 0

getDruid : Dict CharacterID Druid -> CharacterID -> Druid
getDruid druids characterID =
  case Dict.get characterID druids of
    Just druid ->
      druid

    Nothing ->
      { bonusHealing = 0
      , lookingForDefaultBloom = No
      }
