module Analyser.Soul exposing (Model, init, parse, bonusHealing)

import Dict exposing (Dict)
import GenericDict exposing (GenericDict)
import GenericSet exposing (GenericSet)
import Time exposing (Time)

import Legendaries
import WarcraftLogs.Models as WCL exposing (Event(..))

type Model = Model (Dict CharacterID Druid)

type alias CharacterID = Int

type alias Druid =
  { bonusHealing : GenericDict Legendaries.Source Int
  , lastCast : Maybe (Time, Cast)
  , sotf : Maybe (Time, Cast)
  , hots : GenericSet (CharacterID, Int)
  }

type Cast = Rejuvenation CharacterID | WildGrowth

init : Model
init = Model <| Dict.empty

parse : WCL.Event -> Model -> Model
parse event (Model druids) =
  Model <| parse_ event druids

parse_ : WCL.Event -> Dict CharacterID Druid -> Dict CharacterID Druid
parse_ event =
  case event of
    CombatantInfo {sourceID, specID} ->
      onlyIf (specID == 105) <|
        Dict.insert sourceID
          { bonusHealing = GenericDict.empty Legendaries.compareSource
          , lastCast = Nothing
          , sotf = Nothing
          , hots = GenericSet.empty compareTuple
          }

    Cast {timestamp, sourceID, targetID, ability} ->
      usingDruid sourceID <| \druid ->
        let
          cast =
            case (ability.id, targetID) of
              (774, Just target) ->
                Just <| (timestamp, Rejuvenation target)

              (48438, _) ->
                Just <| (timestamp, WildGrowth)

              _ ->
                Nothing
        in
          { druid | lastCast = cast }

    RemoveBuff {timestamp, sourceID, targetID, ability} ->
      onlyIf (ability.id == 114108) <| usingDruid sourceID <| \druid ->
        { druid | sotf = druid.lastCast }

    ApplyBuff {timestamp, sourceID, targetID, ability} ->
      let
        updateHots updateFunc druid =
          { druid | hots = updateFunc (targetID, ability.id) druid.hots }

        removeHot =
          updateHots GenericSet.remove

        addHot =
          updateHots GenericSet.insert

      in
        usingDruid sourceID <| \druid -> druid |>
          case (ability.id, druid.sotf) of
            (774, Just (sotfTime, Rejuvenation rejuvTarget)) ->
              if (sotfTime + 100 > timestamp && rejuvTarget == targetID) then
                addHot
              else
                removeHot

            (155777, Just (sotfTime, Rejuvenation rejuvTarget)) ->
              if (sotfTime + 100 > timestamp && rejuvTarget == targetID) then
                addHot
              else
                removeHot

            (48438, Just (sotfTime, WildGrowth)) ->
              if (sotfTime + 100 > timestamp) then
                addHot
              else
                removeHot

            _ ->
              removeHot

    RefreshBuff data ->
      parse_ (ApplyBuff data)

    Heal {sourceID, targetID, ability, amount, overheal} ->
      usingDruid sourceID <| \druid ->
        if GenericSet.member (targetID, ability.id) druid.hots then
          let
            (source, bonusPercentage) =
              if ability.id == 774 || ability.id == 155777 then
                (Legendaries.Rejuvenation, 200)
              else
                (Legendaries.WildGrowth, 75)

            baseHeal =
              (amount + overheal) * 100 // (100 + bonusPercentage)

            bonusAmount =
              max 0 (amount - baseHeal)

            add current =
              Just <| bonusAmount + Maybe.withDefault 0 current
          in
            { druid
            | bonusHealing = GenericDict.update source add druid.bonusHealing
            }
        else
          druid

    _ ->
      identity

onlyIf : Bool -> (a -> a) -> a -> a
onlyIf cond f =
  if cond then f else identity

usingDruid : CharacterID -> (Druid -> Druid) -> (Dict CharacterID Druid) -> (Dict CharacterID Druid)
usingDruid characterID f =
  Dict.update characterID (Maybe.map f)

bonusHealing : Model -> CharacterID -> Legendaries.BonusHealing
bonusHealing (Model druids) characterID =
  Dict.get characterID druids
    |> Maybe.map .bonusHealing
    |> Maybe.withDefault (GenericDict.empty Legendaries.compareSource)
    |> Legendaries.Breakdown

compareTuple : (CharacterID, Int) -> (CharacterID, Int) -> Order
compareTuple (c1, a1) (c2, a2) =
  case compare c1 c2 of
    EQ ->
      compare a1 a2

    x ->
      x
