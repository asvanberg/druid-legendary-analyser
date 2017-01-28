module Legendaries.Tier19 exposing (Model, init, parse, bonusHealing)

import Dict exposing (Dict)
import Set exposing (Set)
import Time exposing (Time)

import WarcraftLogs.Models exposing (Event, Event(..))

type Model = Model (Dict Int Druid)

type alias Druid =
  { lastRejuvenationTarget : TargetID
  , selfRejuvenation : Maybe (Time, SpellID)
  , hots : Set (SpellID, TargetID)
  , wildGrowths : Dict TargetID Time
  , tearstoneEquipped : Bool
  , bonusHealing : Int
  , lastPotA : Maybe Time
  }

type alias TargetID = Int
type alias SpellID = Int

init : Model
init =
  Model Dict.empty

spells : List Int
spells = [ 774, 155777 ]

parse : Event -> Model -> Model
parse event model =
  case event of
    EncounterStart _ ->
      resetBonusHealing model
    CombatantInfo {sourceID, gear} ->
      if List.any ((==) 137042 << .id) gear then
        let
          druid = getDruid model sourceID
          newDruid =
            { druid
            | tearstoneEquipped = True
            }
        in
          updateDruid sourceID newDruid model
      else
        model
    ApplyBuff {sourceID, targetID, ability, timestamp} ->
      let
        druid = getDruid model sourceID
        correctSpell = List.member ability.id spells
        rejuvenationTarget = targetID == druid.lastRejuvenationTarget
        recentlyWildGrowth =
          case Dict.get targetID druid.wildGrowths of
            Just wgTime ->
              wgTime > timestamp - 10 -- Wild growth applied in the last 10ms
            Nothing ->
              False
        hasTearstone = druid.tearstoneEquipped
        potaApplication =
          case druid.lastPotA of
            Just potaTime ->
              timestamp == potaTime
            Nothing ->
              False
      in
        if correctSpell && not rejuvenationTarget && not potaApplication && not (recentlyWildGrowth && hasTearstone) then
          let
            newDruid =
              { druid
              | hots = Set.insert (ability.id, targetID) druid.hots
              , selfRejuvenation =
                  if targetID == sourceID then Just (timestamp, ability.id)
                  else druid.selfRejuvenation
              }
          in
            updateDruid sourceID newDruid model
        else if ability.id == 48438 then -- Wild growth
          let
            newDruid =
              { druid
              | wildGrowths = Dict.insert targetID timestamp druid.wildGrowths
              }
          in
            updateDruid sourceID newDruid model
        else if correctSpell then
          let
            newDruid =
              { druid
              | hots = Set.remove (ability.id, targetID) druid.hots
              }
          in
            updateDruid sourceID newDruid model
        else
          model
    RefreshBuff eventData ->
      parse (ApplyBuff eventData) model
    Cast {sourceID, targetID, ability, timestamp} ->
      if ability.id == 774 then
        let
          druid = getDruid model sourceID
        in
          if sourceID == (Maybe.withDefault 0 targetID) then
            case druid.selfRejuvenation of
              Just (selfRejuvTime, selfRejuvSpellId) ->
                if selfRejuvTime == timestamp then
                  let
                    newDruid =
                      { druid
                      | hots = Set.remove (selfRejuvSpellId, sourceID) druid.hots
                      }
                  in
                    updateDruid sourceID newDruid model
                else
                  model
              Nothing ->
                model
          else
            let
              newDruid =
                { druid
                | lastRejuvenationTarget = Maybe.withDefault 0 targetID
                }
            in
              updateDruid sourceID newDruid model
      else
        model
    RemoveBuff {sourceID, ability, timestamp} ->
      if ability.id == 189877 then
        let
          druid = getDruid model sourceID
          newDruid =
            { druid
            | lastPotA = Just timestamp
            }
        in
          updateDruid sourceID newDruid model
      else
        model
    Heal {sourceID, targetID, ability, amount} ->
      let
        druid = getDruid model sourceID
        is4pcTarget =
          List.map (\hot -> (hot, targetID)) spells
            |> List.any (flip Set.member druid.hots)
      in
        if Set.member (ability.id, targetID) druid.hots then
          updateDruid sourceID { druid | bonusHealing = druid.bonusHealing + amount } model
        else if ability.id == 189853 && is4pcTarget then -- Dreamwalker
          updateDruid sourceID { druid | bonusHealing = druid.bonusHealing + amount } model
        else
          model
    _ ->
      model

getDruid : Model -> Int -> Druid
getDruid (Model druids) sourceID =
  case Dict.get sourceID druids of
    Just druid ->
      druid
    Nothing ->
      { lastRejuvenationTarget = 0
      , selfRejuvenation = Nothing
      , hots = Set.empty
      , bonusHealing = 0
      , tearstoneEquipped = False
      , wildGrowths = Dict.empty
      , lastPotA = Nothing
      }

updateDruid : Int -> Druid -> Model -> Model
updateDruid sourceID druid (Model druids) =
  Model <| Dict.insert sourceID druid druids

resetBonusHealing : Model -> Model
resetBonusHealing (Model druids) =
  Model <| Dict.map (\_ druid -> { druid | bonusHealing = 0 }) druids

bonusHealing : Model -> Int -> Int
bonusHealing =
  ((<<) .bonusHealing) << getDruid
