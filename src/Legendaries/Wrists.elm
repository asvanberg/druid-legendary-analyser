module Legendaries.Wrists exposing (Model, init, parse, bonusHealing)

import Dict exposing (Dict)
import Time exposing (..)

import Util.List exposing (find)
import Util.Maybe exposing ((?))

import WarcraftLogs.Models exposing (Event, Event(..))

type Model = Model (Dict Int Druid)

type alias Druid =
  { persistence : Int
  , targetHots : Dict Target (Dict Hot Expiration)
  , swiftmendTargets : Dict Target Timestamp
  , bonusHealing : Int
  }

type alias Target = Int
type alias Hot = Int
type alias Expiration = Time
type alias Timestamp = Time
type alias Duration = Time

hots : Dict Hot Duration
hots = Dict.fromList
  [ (200389, 6 * second) -- Cultivation
  , (48438, 7 * second) -- Wild growth
  , (102352, 8 * second) -- Cenarion ward
  , (8936, 12 * second) -- Regrowth
  ]

bonusDuration : Druid -> Hot -> Duration
bonusDuration druid hot =
  if hot == 774 || hot == 155777 then
    (toFloat druid.persistence) * second
  else
    0

init : Model
init = Model Dict.empty

parse : Event -> Model -> Model
parse event model =
  case event of
    EncounterStart _ ->
      let
        (Model druids) = model
        resetBonusHealing _ druid = { druid | bonusHealing = 0}
      in Model <| Dict.map resetBonusHealing druids
    CombatantInfo {sourceID, specID, artifact} ->
      let
        druid = getDruid model sourceID
        persistence = find ((==) 186396 << .spellID) artifact
        rank = Maybe.map .rank persistence
        newDruid = { druid | persistence = rank ? 0 }
      in
        setDruid model sourceID newDruid
    ApplyBuff {timestamp, sourceID, targetID, ability} ->
      case Dict.get ability.id hots of
        Just baseDuration ->
          let
            druid = getDruid model sourceID
            actualDuration = baseDuration + bonusDuration druid ability.id
            expiration = timestamp + actualDuration

            hots = Dict.get targetID druid.targetHots ? Dict.empty
            newHots = Dict.insert ability.id expiration hots
            newTargetHots = Dict.insert targetID newHots druid.targetHots

            newDruid = { druid | targetHots = newTargetHots }
          in
            setDruid model sourceID newDruid
        Nothing ->
          model
    RefreshBuff {timestamp, sourceID, targetID, ability} ->
      case Dict.get ability.id hots of
        Just baseDuration ->
          let
            druid = getDruid model sourceID
            hots = Dict.get targetID druid.targetHots ? Dict.empty
            expiration = Dict.get ability.id hots ? timestamp
            actualDuration = baseDuration + bonusDuration druid ability.id
            maxPandemic = actualDuration * 0.3
            remaining = expiration - timestamp
            pandemicDuration = clamp 0 maxPandemic remaining
            finalDuration = actualDuration + pandemicDuration

            newExpiration = timestamp + finalDuration
            newHots = Dict.insert ability.id newExpiration hots
            newTargetHots = Dict.insert targetID newHots druid.targetHots
            newDruid = { druid | targetHots = newTargetHots }
          in
            setDruid model sourceID newDruid
        Nothing ->
          model
    Cast {timestamp, sourceID, targetID, ability} ->
      case ability.id of
        197721 -> -- Flourish
          let
            druid = getDruid model sourceID
            extend _ expiration =
              if expiration > timestamp then
                expiration + 6
              else
                expiration
            newTargetHots = Dict.map (always <| Dict.map extend) druid.targetHots
            newDruid = { druid | targetHots = newTargetHots }
          in
            setDruid model sourceID newDruid
        18562 -> -- Swiftmend
          case targetID of
            Just target ->
              let
                druid = getDruid model sourceID
                newSwiftmendTargets = Dict.insert target timestamp druid.swiftmendTargets
              in
                setDruid model sourceID { druid | swiftmendTargets = newSwiftmendTargets }
            Nothing ->
              model
        _ ->
          model
    Heal {timestamp, sourceID, targetID, ability, amount} ->
      let
        druid = getDruid model sourceID
        hots = getHots druid targetID
      in
        case Dict.get ability.id hots of
          Just expiration ->
            if expiration < timestamp then
              case Dict.get targetID druid.swiftmendTargets of
                Just lastSwiftmend ->
                  if timestamp - lastSwiftmend < 16 * second then
                    setDruid model sourceID { druid | bonusHealing = druid.bonusHealing + amount }
                  else
                    model
                Nothing ->
                  model
            else
              model
          Nothing ->
            model
    _ ->
      model

bonusHealing : Model -> Int -> Int
bonusHealing =
  ((<<) .bonusHealing) << getDruid

getDruid : Model -> Int -> Druid
getDruid (Model druids) id = Dict.get id druids ? newDruid

newDruid : Druid
newDruid =
  { persistence = 0
  , targetHots = Dict.empty
  , swiftmendTargets = Dict.empty
  , bonusHealing = 0
  }

setDruid : Model -> Int -> Druid -> Model
setDruid (Model druids) id druid =
  Model <| Dict.insert id druid druids

getHots : Druid -> Target -> Dict Hot Expiration
getHots druid target =
  Dict.get target druid.targetHots ? Dict.empty
