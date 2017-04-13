module Analyser.Tearstone exposing (Model, init, parse, bonusHealing)

import Dict exposing (Dict)
import Set exposing (Set)

import Util.List exposing (find)
import Util.Maybe exposing ((?))

import WarcraftLogs.Models as WCL exposing (Event(..))

type Model = Model (Dict Int Druid)

type alias Druid =
  { persistence : Int
  , bonusHealing : Int
  , tearstoneActive : Bool
  , rejuvenationTarget : Int
  , tearstoneTargets : Set (Int, Int) -- (HoT, Target)
  }

init : Model
init =
  Model Dict.empty

parse : WCL.Event -> Model -> Model
parse event (Model druids) =
  let
    blankDruid =
      { persistence = 0
      , bonusHealing = 0
      , tearstoneActive = False
      , rejuvenationTarget = 0
      , tearstoneTargets = Set.empty
      }
    getDruid id = Dict.get id druids ? blankDruid
    updated id druid =
      Model <| Dict.insert id druid druids
    tearstoneSpells = Set.fromList [774, 155777]
  in case event of
    EncounterStart _ ->
      let resetBonusHealing _ druid = { druid | bonusHealing = 0 }
      in Model <| Dict.map resetBonusHealing druids

    CombatantInfo {sourceID, artifact} ->
      let
        druid = getDruid sourceID
        persistence = find ((==) 186396 << .spellID) artifact
        rank = Maybe.map .rank persistence
      in
        updated sourceID { druid | persistence = rank ? 0 }

    Cast {sourceID, ability, targetID} ->
      let
        druid = getDruid sourceID
      in case ability.id of
        48438 -> -- Wild growth
          updated sourceID { druid | tearstoneActive = True }

        774 -> -- Rejuvenation
          updated sourceID { druid | rejuvenationTarget = targetID ? 0 }

        _ ->
          Model druids

    ApplyBuff {sourceID, targetID, ability} ->
      let
        druid = getDruid sourceID
      in
        if Set.member ability.id tearstoneSpells then
          if targetID == druid.rejuvenationTarget then
            let
              tearstoneTargets = Set.remove (ability.id, targetID) druid.tearstoneTargets
              tearstoneActive = False
            in
              updated sourceID
                { druid
                | tearstoneTargets = tearstoneTargets
                , tearstoneActive = tearstoneActive
                }
          else if druid.tearstoneActive then
            updated sourceID
              { druid
              | tearstoneTargets = Set.insert (ability.id, targetID) druid.tearstoneTargets
              }
          else
            let
              tearstoneTargets = Set.remove (ability.id, targetID) druid.tearstoneTargets
            in
              updated sourceID
                { druid
                | tearstoneTargets = tearstoneTargets
                }
        else
          Model druids

    RefreshBuff eventData ->
      parse (ApplyBuff eventData) (Model druids)

    Heal {sourceID, targetID, ability, amount} ->
      let
        druid = getDruid sourceID
        isTearstoneTarget =
          Set.map (\hot -> (hot, targetID)) tearstoneSpells
            |> Set.toList
            |> List.any (flip Set.member druid.tearstoneTargets)
      in
        if Set.member (ability.id, targetID) druid.tearstoneTargets then
          updated sourceID { druid | bonusHealing = druid.bonusHealing + amount }
        else if ability.id == 189853 && isTearstoneTarget then -- Dreamwalker
          updated sourceID { druid | bonusHealing = druid.bonusHealing + amount }
        else
          Model druids

    _ ->
      Model druids

bonusHealing : Model -> Int -> Int
bonusHealing (Model druids) id =
  Dict.get id druids
    |> Maybe.map .bonusHealing
    |> Maybe.withDefault 0
