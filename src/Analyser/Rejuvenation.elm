module Analyser.Rejuvenation exposing (..)

import Analyser.Haste as Haste exposing (Haste)
import Dict exposing (Dict)
import GenericDict exposing (GenericDict)
import Legendaries exposing (BonusHealing(..), Legendary(..), Source(..))
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
  , bonusHealing : GenericDict Legendary (GenericDict Source Int)
  , haste : Haste
  }
type alias Hot =
  { applied : Time
  , expiration : Time
  , lastTick : Time
  , numShoulderTicks : Int
  , effects : List (Effect, Time)
  }
type Model = Model (Dict CharacterID Druid)

isRejuvenation : AbilityID -> Bool
isRejuvenation abilityID =
  abilityID == 774 || abilityID == 155777

baseDuration : Druid -> AbilityID -> Time
baseDuration druid abilityID =
  if isRejuvenation abilityID then
    toFloat (15 + druid.persistence) * second
  else if abilityID == 48438 then
    7 * second
  else
    0

init : Model
init = Model Dict.empty

parse : WCL.Event -> Model -> Model
parse event (Model druids) =
  Model <| parse_ event druids

parse_ : WCL.Event -> Dict CharacterID Druid -> Dict CharacterID Druid
parse_ event druids =
  druids |> case event of
    EncounterStart _ ->
      let
        resetBonusHealing _ druid =
          { druid | bonusHealing = GenericDict.empty Legendaries.compareLegendary }
      in
        Dict.map resetBonusHealing

    CombatantInfo ({sourceID, specID, artifact, gear, spellHasteRating, strength} as info) ->
      usingDruid sourceID <| \druid ->
        let
          persistence =
            find ((==) 186396 << .spellID) artifact
          rank =
            Maybe.map .rank persistence
          isEquipped itemId =
            List.any ((==) itemId << .id) gear
        in
          { druid
          | persistence = rank ? 0
          , deepRooted =
            isDefined <| find ((==) 238122 << .spellID) artifact
          , shoulders = isEquipped 137072
          , bracers = isEquipped 137095
          , haste = Haste.init info
          }

    Cast {sourceID, ability} ->
      usingDruid sourceID <| \druid ->
        case ability.id of
          197721 -> -- Flourish
            let
              addFlourishEffect hot =
                { hot
                | effects = hot.effects ++ [(Flourish, 6)]
                }
            in
              { druid
              | hots = Dict.map (always addFlourishEffect) druid.hots
              }
          18562 -> -- Swiftmend
            let
              addBracerEffect hot =
                { hot
                | effects = hot.effects ++ [(Bracer, 10)]
                }
            in
              if druid.bracers then
                { druid
                | hots = Dict.map (always addBracerEffect) druid.hots
                }
              else
                druid
          _ ->
            druid

    ApplyBuff {timestamp, sourceID, targetID, ability} ->
      if isRejuvenation ability.id || ability.id == 48438 then
        usingDruid sourceID <| \druid ->
          let
            duration =
              baseDuration druid ability.id

            hot =
              { applied = timestamp
              , expiration = timestamp + duration
              , numShoulderTicks = 0
              , lastTick = timestamp
              , effects = [(Base, duration)]
              }
          in
            { druid | hots = Dict.insert (targetID, ability.id) hot druid.hots }
      else
        identity

    RefreshBuff ({timestamp, sourceID, targetID, ability} as eventData) ->
      if isRejuvenation ability.id || ability.id == 48438 then
        let
          druid = getDruid druids sourceID
          maybeHot = getHot druid ability.id targetID
        in
          case maybeHot of
            Nothing ->
              parse_ (ApplyBuff eventData)
            Just hot ->
              let
                newHot =
                  refreshHot timestamp (baseDuration druid ability.id) hot
                newDruid =
                  { druid
                  | hots = Dict.insert (targetID, ability.id) newHot druid.hots
                  }
              in
                Dict.insert sourceID newDruid
      else
        identity

    RemoveBuff {sourceID, targetID, ability} ->
      usingDruid sourceID <| \druid ->
        { druid
        | hots = Dict.remove (targetID, ability.id) druid.hots
        }

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
            , effects = hot.effects ++ [(Tick, durationOfTick)]
            }
          else
            hot
        addDeepRooted remaining hot =
          if hitPoints < (maxHitPoints * 35 // 100) && druid.deepRooted then
            let
              duration =
                calculateDeepRootedDuration (baseDuration druid ability.id) druid.haste

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
            if ability.id == 189853 then
              let
                newDruid =
                  assignDreamwalker druid timestamp targetID amount
              in
                Dict.insert sourceID newDruid
            else
              identity

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
                    Just Legendaries.Shoulders

                  Just DeepRooted ->
                    Just Legendaries.DeepRooted

                  _ ->
                    Nothing

              remaining =
                getRemaining newEffects

              updateHotEffects hot =
                { hot | effects = newEffects }

              updateHot =
                if isRejuvenation ability.id then
                  updateHotEffects >> addShoulderTick >> addDeepRooted remaining >> registerLastTick
                else
                  updateHotEffects >> addDeepRooted remaining >> registerLastTick

              newHot = hot
                |> updateHot

              newDruid =
                { druid
                | hots = Dict.insert (targetID, ability.id) newHot druid.hots
                }

              druidWithBonusHealing =
                case source of
                  Just s ->
                    { newDruid
                    | bonusHealing = addBonusHealing amount s (if isRejuvenation ability.id then Legendaries.Rejuvenation else Legendaries.WildGrowth) newDruid.bonusHealing
                    }

                  Nothing ->
                    newDruid
            in
              Dict.insert sourceID druidWithBonusHealing

    _ ->
      identity

usingDruid : CharacterID -> (Druid -> Druid) -> (Dict CharacterID Druid) -> (Dict CharacterID Druid)
usingDruid characterID updateFunction druids =
  Dict.insert characterID (updateFunction <| getDruid druids characterID) druids

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

refreshHot : Time -> Time -> Hot -> Hot
refreshHot timestamp baseDuration ({expiration, effects, lastTick} as hot) =
  let
    remaining = (getRemaining effects) - (timestamp - lastTick)
    maxPandemic = 0.3 * baseDuration
    pandemicBonus = clamp 0 maxPandemic remaining
    duration = baseDuration + pandemicBonus
  in
    { hot
    | applied = timestamp
    , expiration = timestamp + duration
    , numShoulderTicks = 0
    , lastTick = timestamp
    , effects = []
    }

assignDreamwalker : Druid -> Time -> CharacterID -> Int -> Druid
assignDreamwalker druid timestamp targetID amount =
  let
    getNextEffect hot =
      let
        elapsed =
          timestamp - hot.lastTick
      in
        case hot.effects of
          (effect, remaining) :: rest ->
            if remaining >= elapsed then
              Just effect
            else
              Maybe.map Tuple.first <| List.head rest
          [] ->
            Nothing

    rejuvHot =
      getHot druid 774 targetID

    germHot =
      getHot druid 155777 targetID

    effect =
      case (rejuvHot, germHot) of
        (Just rejuv, Just germ) ->
          if rejuv.applied < germ.applied then
            getNextEffect rejuv |> orElse (getNextEffect germ)
          else
            getNextEffect germ |> orElse (getNextEffect rejuv)

        (Just rejuv, Nothing) ->
          getNextEffect rejuv

        (Nothing, Just germ) ->
          getNextEffect germ

        (Nothing, Nothing) ->
          Nothing

    source =
      case effect of
        Just Tick ->
          Just Legendaries.Shoulders

        Just DeepRooted ->
          Just Legendaries.DeepRooted

        _ ->
          Nothing
  in
    case source of
      Just s ->
        { druid
        | bonusHealing = addBonusHealing amount s Legendaries.Dreamwalker druid.bonusHealing
        }

      Nothing ->
        druid

addBonusHealing
  : Int
  -> Legendary
  -> Source
  -> GenericDict Legendary (GenericDict Source Int)
  -> GenericDict Legendary (GenericDict Source Int)
addBonusHealing amount legendary source =
  let
    increaseSource =
      GenericDict.update source (Maybe.map ((+) amount) >> orElse (Just amount))
    increaseLegendary =
      GenericDict.update legendary (Maybe.map increaseSource >> orElse (Just <| GenericDict.singleton Legendaries.compareSource source amount))
  in
    increaseLegendary

bonusHealing : Model -> Legendary -> CharacterID -> BonusHealing
bonusHealing (Model druids) source druid =
  Breakdown
    <| Maybe.withDefault (GenericDict.empty Legendaries.compareSource)
    <| GenericDict.get source
    <| .bonusHealing
    <| getDruid druids druid

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
        , bonusHealing = GenericDict.empty Legendaries.compareLegendary
        , haste = Haste.unknown
        }

getHot : Druid -> AbilityID -> CharacterID -> Maybe Hot
getHot druid abilityID characterID =
  Dict.get (characterID, abilityID) druid.hots
