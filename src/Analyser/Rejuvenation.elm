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
          { druid | bonusHealing = GenericDict.empty Legendaries.compareLegendary }
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

    RemoveBuff {sourceID, targetID, ability} ->
      let
        druid =
          getDruid druids sourceID

        newDruid =
          { druid
          | hots = Dict.remove (targetID, ability.id) druid.hots
          }
      in
        Dict.insert sourceID newDruid druids

    Cast {sourceID, ability} ->
      case ability.id of
        197721 -> -- Flourish
          let
            druid =
              getDruid druids sourceID
            addFlourishEffect hot =
              { hot
              | effects = hot.effects ++ [(Flourish, 6)]
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
            if ability.id == 189853 then
              let
                newDruid =
                  assignDreamwalker druid timestamp targetID amount
              in
                Dict.insert sourceID newDruid druids
            else
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
                    Just Legendaries.Shoulders

                  Just DeepRooted ->
                    Just Legendaries.DeepRooted

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
                    | bonusHealing = addBonusHealing amount s Legendaries.Rejuvenation newDruid.bonusHealing
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
