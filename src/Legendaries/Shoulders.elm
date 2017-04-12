module Legendaries.Shoulders exposing (..)

import Dict exposing (Dict)
import Legendaries.Haste as Haste exposing (Haste)
import Time exposing (Time, second)
import Util.List exposing (find)
import Util.Maybe exposing ((?), isDefined, orElse)

import WarcraftLogs.Models as WCL exposing (Event(..))

type alias CharacterID = Int
type alias AbilityID = Int
type Effect
  = Base
  | Tick
  | Bracer
  | Flourish
  | DeepRooted
type alias Druid =
  { persistence : Int
  , deepRooted : Bool
  , bracers : Bool
  , shoulders : Bool
  , hots : Dict (CharacterID, AbilityID) Hot
  , bonusHealing : Dict Int Int
  , haste : Haste
  }
type alias Hot =
  { applied : Time
  , expiration : Time
  , bonusDuration : Time
  , lastTick : Time
  , numShoulderTicks : Int
  , effects : List (Effect, Time)
  }
type Model = Model (Dict CharacterID Druid)

type Source
  = Shoulders
  | DeepRootedTrait

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
          { druid | bonusHealing = Dict.empty }
      in
        Dict.map resetBonusHealing druids
    CombatantInfo ({sourceID, specID, artifact, gear, spellHasteRating, strength} as info) ->
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
          , deepRooted =
            isDefined <| find ((==) 238122 << .spellID) artifact
          , shoulders = isEquipped 137072
          , bracers = isEquipped 137095
          , haste = Haste.init info
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
            , effects = [(Base, baseDuration druid)]
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
              | effects = hot.effects ++ [(Flourish, 6)]
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
              | effects = hot.effects ++ [(Bracer, 10)]
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
            , effects = hot.effects ++ [(Tick, durationOfTick)]
            }
          else
            hot
        addDeepRooted remaining hot =
          if hitPoints < (maxHitPoints * 35 // 100) && druid.deepRooted then
            let
              duration =
                calculateDeepRootedDuration (baseDuration druid) druid.haste

              bonusDuration =
                duration - remaining
            in
              if bonusDuration > 0 then
                { hot
                | effects = hot.effects ++ [(DeepRooted, bonusDuration)]
                }
              else
                hot
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
              elapsed =
                timestamp - hot.lastTick

              newEffects =
                updateEffectList elapsed hot.effects

              effectSource =
                Maybe.map Tuple.first
                  <| (List.head newEffects |> orElse (List.head hot.effects))

              source =
                case effectSource of
                  Just Tick ->
                    Just Shoulders

                  Just DeepRooted ->
                    Just DeepRootedTrait

                  _ ->
                    Nothing

              remaining =
                getRemaining newEffects

              updateHotEffects hot =
                { hot | effects = newEffects }

              newHot = hot
                |> updateHotEffects
                |> addShoulderTick
                |> addDeepRooted remaining
                |> registerLastTick

              newDruid =
                { druid
                | hots = Dict.insert (targetID, ability.id) newHot druid.hots
                }

              druidWithBonusHealing =
                case source of
                  Just s ->
                    { newDruid
                    | bonusHealing = Dict.update (asInt s) (Maybe.map ((+) amount) >> orElse (Just amount)) newDruid.bonusHealing
                    }

                  Nothing ->
                    newDruid
            in
              Dict.insert sourceID druidWithBonusHealing druids

    _ ->
      druids

updateEffectList : Time -> List (Effect, Time) -> List (Effect, Time)
updateEffectList elapsed effects =
  case effects of
    [] ->
      []

    (effect, remaining) :: rest ->
      if remaining > elapsed then
        (effect, (remaining - elapsed)) :: rest
      else
        updateEffectList (elapsed - remaining) rest

getRemaining : List (Effect, Time) -> Time
getRemaining =
  List.sum << List.map Tuple.second

calculateDeepRootedDuration : Time -> Haste -> Time
calculateDeepRootedDuration duration haste =
  let
    hasteMultiplier =
      Haste.totalHaste haste

    tickDuration =
      (3 * second) / hasteMultiplier

    durationPlusTick =
      duration + tickDuration

    numTicks =
      round <| durationPlusTick / tickDuration
  in
    (toFloat numTicks) * tickDuration

refreshHot : Time -> Druid -> Hot -> Hot
refreshHot timestamp druid ({expiration, effects, lastTick} as hot) =
  let
    remaining = (getRemaining effects) - (timestamp - lastTick)
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

bonusHealing : Model -> Source -> CharacterID -> Int
bonusHealing (Model druids) source druid =
  Maybe.withDefault 0 <| Dict.get (asInt source) <| .bonusHealing <| getDruid druids druid

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
        , deepRooted = False
        , hots = Dict.empty
        , bonusHealing = Dict.empty
        , haste = Haste.unknown
        }

getHot : Druid -> AbilityID -> CharacterID -> Maybe Hot
getHot druid abilityID characterID =
  Dict.get (characterID, abilityID) druid.hots

asInt : Source -> Int
asInt source =
  case source of
    Shoulders ->
      0

    DeepRootedTrait ->
      1
