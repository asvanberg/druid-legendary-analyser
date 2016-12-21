module Legendaries.Shoulders exposing (Model, init, parse, bonusHealing)

import Dict exposing (Dict)
import Time exposing (Time, second)

import Util.List exposing (find)
import Util.Maybe exposing ((?))

import WarcraftLogs.Models exposing (Event, Event(..))

type Model = Model (Dict Int Druid)

type alias Druid =
  { persistence : Int
  , targetHots : Dict Target (Dict Hot Expiration)
  , bonusHealing : Int
  }

type alias Target = Int
type alias Hot = Int
type alias Expiration = Time
type alias Duration = Time

hots : Dict Hot Duration
hots = Dict.fromList
  [ (774, 15 * second)
  , (155777, 15 * second)
  ]

bonusDuration : Druid -> Duration
bonusDuration druid =
  toFloat druid.persistence * second

init : Model
init =
  Model Dict.empty

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
            actualDuration = baseDuration + bonusDuration druid
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
            actualDuration = baseDuration + bonusDuration druid
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
    Cast {timestamp, sourceID, ability} ->
      if ability.id == 197721 then -- Flourish
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
      else
        model
    Heal {timestamp, sourceID, targetID, ability, amount} ->
      let
        druid = getDruid model sourceID
        hots = Dict.get targetID druid.targetHots ? Dict.empty
      in
        if ability.id == 189853 then -- Dreamwalker
          let
            dreamwalkerSpells = [774, 155777]
            isExpired hotId =
              Dict.get hotId hots
                |> Maybe.map (\expiration -> expiration < timestamp)
                |> Maybe.withDefault True
          in
            if List.all isExpired dreamwalkerSpells then
              setDruid model sourceID { druid | bonusHealing = druid.bonusHealing + amount }
            else
              model
        else
          case Dict.get ability.id hots of
            Just expiration ->
              if expiration < timestamp then
                setDruid model sourceID { druid | bonusHealing = druid.bonusHealing + amount }
              else
                model
            Nothing ->
              model
    _ ->
      model

bonusHealing : Model -> Int -> Int
bonusHealing =
  ((<<) .bonusHealing) << getDruid

setDruid : Model -> Int -> Druid -> Model
setDruid (Model model) sourceID druid =
  let
    newDruids = Dict.insert sourceID druid model
  in Model newDruids

getDruid : Model -> Int -> Druid
getDruid (Model model) sourceID =
  Maybe.withDefault emptyDruid <| Dict.get sourceID model

emptyDruid : Druid
emptyDruid =
  { persistence = 0
  , targetHots = Dict.empty
  , bonusHealing = 0
  }

getDuration : Druid -> Float
getDuration druid =
  (toFloat druid.persistence + 15) * second
