module Analyser.Tearstone exposing (Model, init, parse, bonusHealing)

import Dict exposing (Dict)
import GenericDict exposing (GenericDict)
import Set exposing (Set)

import Legendaries

import Util.List exposing (find)
import Util.Maybe exposing ((?), orElse)

import WarcraftLogs.Models as WCL exposing (Event(..))

type Model = Model (Dict Int Druid)

type alias Druid =
  { persistence : Int
  , bonusHealing : GenericDict Legendaries.Source Int
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
    updated id druid =
      Model <| Dict.insert id druid druids
    tearstoneSpells = Set.fromList [774, 155777]
  in case event of
    EncounterStart _ ->
      let
        resetBonusHealing _ druid =
          { druid | bonusHealing = GenericDict.empty Legendaries.compareSource }
      in Model <| Dict.map resetBonusHealing druids

    CombatantInfo {sourceID, artifact} ->
      let
        druid = getDruid druids sourceID
        persistence = find ((==) 186396 << .spellID) artifact
        rank = Maybe.map .rank persistence
      in
        updated sourceID { druid | persistence = rank ? 0 }

    Cast {sourceID, ability, targetID} ->
      let
        druid = getDruid druids sourceID
      in case ability.id of
        48438 -> -- Wild growth
          updated sourceID { druid | tearstoneActive = True }

        774 -> -- Rejuvenation
          updated sourceID { druid | rejuvenationTarget = targetID ? 0 }

        _ ->
          Model druids

    ApplyBuff {sourceID, targetID, ability} ->
      let
        druid = getDruid druids sourceID
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
        druid = getDruid druids sourceID
        isTearstoneTarget =
          Set.map (\hot -> (hot, targetID)) tearstoneSpells
            |> Set.toList
            |> List.any (flip Set.member druid.tearstoneTargets)
      in
        if Set.member (ability.id, targetID) druid.tearstoneTargets then
          updated sourceID { druid | bonusHealing = addBonusHealing amount Legendaries.Rejuvenation druid.bonusHealing }
        else if ability.id == 189853 && isTearstoneTarget then -- Dreamwalker
          updated sourceID { druid | bonusHealing = addBonusHealing amount Legendaries.Dreamwalker druid.bonusHealing }
        else
          Model druids

    _ ->
      Model druids

getDruid : Dict Int Druid -> Int -> Druid
getDruid druids sourceID =
  let
    blankDruid =
      { persistence = 0
      , bonusHealing = GenericDict.empty Legendaries.compareSource
      , tearstoneActive = False
      , rejuvenationTarget = 0
      , tearstoneTargets = Set.empty
      }
  in
    Dict.get sourceID druids ? blankDruid

addBonusHealing
  : Int
  -> Legendaries.Source
  -> GenericDict Legendaries.Source Int
  -> GenericDict Legendaries.Source Int
addBonusHealing amount source =
  GenericDict.update source (Maybe.map ((+) amount) >> orElse (Just amount))

bonusHealing : Model -> Int -> Legendaries.BonusHealing
bonusHealing (Model druids) sourceID =
  let
    sources =
      .bonusHealing <| getDruid druids sourceID
  in
    Legendaries.Breakdown sources
