module Legendaries.Shoulders exposing (..)

import Dict exposing (Dict)
import Time exposing (Time, second)
import Util.List exposing (find)
import Util.Maybe exposing ((?))

import WarcraftLogs.Models as WCL exposing (Event(..))

type alias CharacterID = Int
type alias AbilityID = Int
type Effect = Tick Time | Bracer Time | Flourish Time
type alias Druid =
  { persistence : Int
  , bracers : Bool
  , shoulders : Bool
  , hots : Dict (CharacterID, AbilityID) Hot
  , bonusHealing : Int
  }
type alias Hot =
  { applied : Time
  , expiration : Time
  , bonusDuration : Time
  , lastTick : Time
  , numShoulderTicks : Int
  , effects : List Effect
  }
type Model = Model (Dict CharacterID Druid)

rejuvenationId : Int
rejuvenationId = 774

baseDuration : Druid -> Time
baseDuration druid =
  toFloat (15 + druid.persistence) * second

init : Model
init = Model Dict.empty

parse : WCL.Event -> Model -> Model
parse event (Model druids) =
  Model <| parse_ event druids

parse_ : WCL.Event -> Dict CharacterID Druid -> Dict CharacterID Druid
parse_ event druids =
  case event of
    EncounterStart _ ->
      let
        resetBonusHealing _ druid =
          { druid | bonusHealing = 0}
      in
        Dict.map resetBonusHealing druids
    CombatantInfo {sourceID, specID, artifact, gear} ->
      let
        druid =
          getDruid druids sourceID
        persistence =
          find ((==) 186396 << .spellID) artifact
        rank =
          Maybe.map .rank persistence
        isEquipped itemId =
          List.any ((==) itemId << .id) gear
        newDruid =
          { druid
          | persistence = rank ? 0
          , shoulders = isEquipped 137072
          , bracers = isEquipped 137095
          }
      in
        Dict.insert sourceID newDruid druids
    ApplyBuff {timestamp, sourceID, targetID, ability} ->
      if ability.id == 774 || ability.id == 155777 then
        let
          druid = getDruid druids sourceID
          hot =
            { applied = timestamp
            , expiration = timestamp + baseDuration druid
            , bonusDuration = 0
            , numShoulderTicks = 0
            , lastTick = timestamp
            , effects = []
            }
          newDruid =
            { druid | hots = Dict.insert (targetID, ability.id) hot druid.hots }
        in
          Dict.insert sourceID newDruid druids
      else
        druids

    RefreshBuff ({timestamp, sourceID, targetID, ability} as eventData) ->
      if ability.id == 774 || ability.id == 155777 then
        let
          druid = getDruid druids sourceID
          maybeHot = getHot druid ability.id targetID
        in
          case maybeHot of
            Nothing ->
              parse_ (ApplyBuff eventData) druids
            Just hot ->
              let
                newHot =
                  refreshHot timestamp druid hot
                newDruid =
                  { druid
                  | hots = Dict.insert (targetID, ability.id) newHot druid.hots
                  }
              in
                Dict.insert sourceID newDruid druids
      else
        druids

    Cast {sourceID, ability} ->
      case ability.id of
        197721 -> -- Flourish
          let
            druid =
              getDruid druids sourceID
            addFlourishEffect hot =
              { hot
              | effects = hot.effects ++ [Flourish 6]
              , bonusDuration = hot.bonusDuration + 6 * second
              }
            newDruid =
              { druid
              | hots = Dict.map (always addFlourishEffect) druid.hots
              }
          in
            Dict.insert sourceID newDruid druids
        18562 -> -- Swiftmend
          let
            druid =
              getDruid druids sourceID
            addBracerEffect hot =
              { hot
              | effects = hot.effects ++ [Bracer 10]
              , bonusDuration = hot.bonusDuration + 10 * second
              }
            newDruid =
              { druid
              | hots = Dict.map (always addBracerEffect) druid.hots
              }
          in
            if druid.bracers then
              Dict.insert sourceID newDruid druids
            else
              druids
        _ ->
          druids

    Heal {timestamp, sourceID, targetID, ability, amount, hitPoints, maxHitPoints} ->
      let
        druid =
          getDruid druids sourceID
        durationOfTick =
          (3 * second)
        addShoulderTick hot =
          if druid.shoulders && hot.numShoulderTicks < 5 && (hitPoints - amount) == maxHitPoints then
            { hot
            | numShoulderTicks = hot.numShoulderTicks + 1
            , bonusDuration = hot.bonusDuration + durationOfTick
            , effects = hot.effects ++ [Tick durationOfTick]
            }
          else
            hot
        registerLastTick hot =
          { hot | lastTick = timestamp }
        maybeHot =
          getHot druid ability.id targetID
      in
        case maybeHot of
          Nothing ->
            druids

          Just hot ->
            let
              (hot0, gaveBonus) =
                handleEffect timestamp hot
              hot1 =
                (addShoulderTick << registerLastTick) hot0
              bonusHealing =
                if gaveBonus then amount else 0
              newDruid =
                { druid
                | bonusHealing = druid.bonusHealing + bonusHealing
                , hots = Dict.insert (targetID, ability.id) hot1 druid.hots
                }
            in
              Dict.insert sourceID newDruid druids
    _ ->
      druids

-- TODO: Remove duplication
handleEffect : Time -> Hot -> (Hot, Bool)
handleEffect timestamp ({applied, expiration, effects, lastTick} as hot) =
  if timestamp < expiration then
    (hot, False)
  else
    case effects of
      [] ->
        (hot, False)

      (Tick durationLeft) :: rest ->
        let
          timePassed =
            timestamp - (max lastTick expiration)
          newDurationLeft =
            durationLeft - timePassed
        in
          if newDurationLeft >= 0 then
            ({ hot | effects = (Tick newDurationLeft) :: rest }, True)
          else
            overlapNextEffect timestamp rest hot (-newDurationLeft)

      (Flourish durationLeft) :: rest ->
        let
          timePassed =
            timestamp - (max lastTick expiration)
          newDurationLeft =
            durationLeft - timePassed
        in
          if newDurationLeft >= 0 then
            ({hot | effects = (Flourish newDurationLeft) :: rest}, False)
          else
            overlapNextEffect timestamp rest hot (-newDurationLeft)

      (Bracer durationLeft) :: rest ->
        let
          timePassed =
            timestamp - (max lastTick expiration)
          newDurationLeft =
            durationLeft - timePassed
        in
          if newDurationLeft >= 0 then
            ({hot | effects = (Bracer newDurationLeft) :: rest}, False)
          else
            overlapNextEffect timestamp rest hot (-newDurationLeft)

-- TODO: Remove duplication
overlapNextEffect : Time -> List Effect -> Hot -> Time -> (Hot, Bool)
overlapNextEffect timestamp rest ({expiration} as hot) overlappingDuration =
  case rest of
    [] ->
      (hot, False)

    (Tick durationLeft) :: rest ->
      ({ hot | effects = (Tick <| durationLeft - overlappingDuration) :: rest }, True)

    (Flourish durationLeft) :: rest ->
      ({hot | effects = (Flourish <| durationLeft - overlappingDuration) :: rest }, False)

    (Bracer durationLeft) :: rest ->
      ({hot | effects = (Bracer <| durationLeft - overlappingDuration) :: rest }, False)

refreshHot : Time -> Druid -> Hot -> Hot
refreshHot timestamp druid ({expiration, bonusDuration} as hot) =
  let
    remaining = expiration + bonusDuration - timestamp
    maxPandemic = 0.3 * baseDuration druid
    pandemicBonus = clamp 0 maxPandemic remaining
    duration = baseDuration druid + pandemicBonus
  in
    { hot
    | applied = timestamp
    , expiration = timestamp + duration
    , bonusDuration = 0
    , numShoulderTicks = 0
    , lastTick = timestamp
    , effects = []
    }

bonusHealing : Model -> CharacterID -> Int
bonusHealing (Model druids) druid =
  .bonusHealing <| getDruid druids druid

getDruid : Dict CharacterID Druid -> CharacterID -> Druid
getDruid druids characterID =
  let
    maybeDruid = Dict.get characterID druids
  in
    case maybeDruid of
      Just druid ->
        druid
      Nothing ->
        { bracers = False
        , shoulders = False
        , persistence = 0
        , hots = Dict.empty
        , bonusHealing = 0
        }

getHot : Druid -> AbilityID -> CharacterID -> Maybe Hot
getHot druid abilityID characterID =
  Dict.get (characterID, abilityID) druid.hots
