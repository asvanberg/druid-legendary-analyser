module Legendaries.Haste exposing (Haste, init, totalHaste, unknown)

import Dict exposing (Dict)

import WarcraftLogs.Models as WCL exposing (Aura, Event, Event(..), Item)

type alias Haste =
  { nelf : Bool
  , rating : Int
  , effects : List TemporaryEffect
  , itemLevels : Dict ItemID ItemLevel
  }

type alias ItemID = Int
type alias ItemLevel = Int

type TemporaryEffect
  = Heroism
  | ChronoShard
  | Map
  | Berserking

-- Multiplier is in percent (so heroism is 30 for example)
type HasteEffect = Multiplier Int | RatingBonus Int

init : { c | gear : List Item, spellHasteRating : Int, strength : Int, auras : List Aura } -> Haste
init { spellHasteRating, gear, strength, auras } =
  let
    nelf =
      strength == 4405

    itemLevels =
      Dict.fromList <|
        List.map (\{id, itemLevel} -> (id, itemLevel)) gear

    effects =
      List.filterMap (isTemporaryEffect << .id) auras
  in
    { nelf = nelf
    , rating = spellHasteRating
    , effects = effects
    , itemLevels = itemLevels
    }

unknown : Haste
unknown =
  { nelf = False
  , rating = 0
  , effects = []
  , itemLevels = Dict.empty
  }

update : WCL.Event -> Haste -> Haste
update event haste =
  case event of
    ApplyBuff {ability} ->
      case isTemporaryEffect ability.id of
        Just effect ->
          { haste
          | effects = effect :: haste.effects
          }
        Nothing ->
          haste
    RemoveBuff {ability} ->
      case isTemporaryEffect ability.id of
        Just effect ->
          { haste
          | effects = List.filter ((/=) effect) haste.effects
          }
        Nothing ->
          haste
    _ ->
      haste

isTemporaryEffect : Int -> Maybe TemporaryEffect
isTemporaryEffect abilityID =
  case abilityID of
    32182 ->
      Just Heroism
    90355 ->
      Just Heroism
    160452 ->
      Just Heroism
    80353 ->
      Just Heroism
    2825 ->
      Just Heroism
    214128 ->
      Just ChronoShard
    225753 ->
      Just Map
    26297 ->
      Just Berserking
    _ ->
      Nothing

toHasteEffect : Dict ItemID ItemLevel -> TemporaryEffect -> HasteEffect
toHasteEffect itemLevels abilityID =
  case abilityID of
    Heroism -> -- Heroism
      Multiplier 30
    ChronoShard -> -- Chrono shard
      case Dict.get 137419 itemLevels of
        Just itemLevel ->
          RatingBonus <| chronoShardRating itemLevel
        Nothing ->
          Debug.log "Gained chrono shard without chrono shard equipped" <| RatingBonus 0
    Map -> -- Map (Dragon)
      case Dict.get 140803 itemLevels of
        Just itemLevel ->
          RatingBonus <| mapRating itemLevel
        Nothing ->
          Debug.log "Gained map without map equipped" <| RatingBonus 0
    Berserking ->
      Multiplier 15

-- Returns the total haste multiplier (1.20 for 20% haste)
totalHaste : Haste -> Float
totalHaste {nelf, rating, effects, itemLevels} =
  let
    split effect =
      case effect of
        Multiplier multiplier ->
          (0, 1 + (toFloat multiplier) / 100)
        RatingBonus rating ->
          (rating, 1)

    combine (rb1, m1) (rb2, m2) =
      (rb1 + rb2, m1 * m2)

    (bonusRating, effectFactor) =
      List.foldl combine (0, 1) <| List.map (split << toHasteEffect itemLevels) effects

    ratingFactor =
      1 + ((toFloat <| rating + bonusRating) / 375 / 100)

    nelfFactor =
      if nelf then 1.01 else 1
  in
    nelfFactor * ratingFactor * effectFactor

mapRating : Int -> Int
mapRating itemLevel =
  estimateSecondary (toFloat itemLevel - 860) 3681

chronoShardRating : Int -> Int
chronoShardRating itemLevel =
  estimateSecondary (toFloat itemLevel - 825) 4890

estimateSecondary : Float -> Float -> Int
estimateSecondary dIlvl baseValue =
    round <| baseValue * 1.15 ^ (dIlvl / 15) * secondaryMultiplier ^ dIlvl

secondaryMultiplier : Float
secondaryMultiplier = 0.994435486
