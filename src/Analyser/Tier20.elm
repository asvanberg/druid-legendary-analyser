module Analyser.Tier20 exposing (Model, init, parse, bonusHealing)

import Dict exposing (Dict)

import WarcraftLogs.Models as WCL exposing (Event(..))

type alias CharacterID = Int

type Model = Model (Dict CharacterID Druid)

type alias Druid =
  { active : Bool
  , bonusHealing : Int
  }

efflorescenceBonus : Int
efflorescenceBonus = 200

init : Model
init = Model Dict.empty

parse : WCL.Event -> Model -> Model
parse event (Model druids) =
  Model <| parse_ event druids

parse_ : WCL.Event -> Dict Int Druid -> Dict Int Druid
parse_ event =
  case event of
    CombatantInfo {sourceID, specID} ->
      onlyIf (specID == 105) <|
        Dict.insert sourceID { active = False, bonusHealing = 0 }

    ApplyBuff {sourceID, ability} ->
      onlyIf (ability.id == 242315) <|
        usingDruid sourceID (\druid -> { druid | active = True })

    RemoveBuff {sourceID, ability} ->
      onlyIf (ability.id == 242315) <|
        usingDruid sourceID (\druid -> { druid | active = False })

    Heal {sourceID, ability, amount, overheal} ->
      usingDruid sourceID <| \druid ->
        if ability.id == 81269 && druid.active then
          let
            totalHeal =
              amount + overheal

            baseHeal =
              totalHeal * 100 // (100 + efflorescenceBonus)

            bonusHealing =
              max 0 (amount - baseHeal)
          in
            { druid | bonusHealing = druid.bonusHealing + bonusHealing }

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

bonusHealing : Model -> CharacterID -> Int
bonusHealing (Model druids) characterID =
  Maybe.withDefault 0 <| Maybe.map .bonusHealing <| Dict.get characterID druids
