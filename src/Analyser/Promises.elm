module Analyser.Promises exposing (Model, init, parse, bonusHealing)

import Dict exposing (Dict)

import Util.List exposing (find)

import WarcraftLogs.Models as WCL exposing (Event(..))

type Model = Model (Dict CharacterID Druid)

type alias CharacterID = Int
type alias Druid =
  { savings : Int
  , usedSavings : Int
  , promisesItemLevel : Maybe Int
  , promisesBuff : Maybe Promises
  }
type Promises
  = Ace
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight

init : Model
init = Model Dict.empty

parse : WCL.Event -> Model -> Model
parse event (Model druids) =
  Model <| parse_ event druids

parse_ : WCL.Event -> Dict CharacterID Druid -> Dict CharacterID Druid
parse_ event =
  case event of
    CombatantInfo {sourceID, specID, gear} ->
      onlyIf (specID == 105) <|
        Dict.insert sourceID
          { savings = 0
          , usedSavings = 0
          , promisesItemLevel = scanForPromises gear
          , promisesBuff = Nothing
          }

    ApplyBuff {sourceID, ability} ->
      usingDruid sourceID <| \druid ->
        case getPromisesBuff ability.id of
          Just buff ->
            { druid | promisesBuff = Just buff }

          Nothing ->
            druid

    Cast {sourceID, resources} ->
      case find ((==) 0 << .type_) resources of
        Just resource ->
          usingDruid sourceID <| \druid ->
            case resource.cost of
              Just cost ->
                let
                  newSavings =
                    druid.savings + (getReduction druid)

                  manaLeftAfterCast =
                    resource.amount - cost

                  savingsUsed =
                    newSavings - manaLeftAfterCast
                in
                  if savingsUsed > 0 then
                    { druid
                    | savings = newSavings - savingsUsed
                    , usedSavings = druid.usedSavings + savingsUsed
                    }
                  else
                    { druid | savings = newSavings }

              Nothing ->
                druid

        Nothing ->
          identity

    _ ->
      identity

getReduction : Druid -> Int
getReduction druid =
  case (druid.promisesItemLevel, druid.promisesBuff) of
    (Just ilvl, Just buff) ->
      let
        baseReduction =
          case buff of
            Ace ->
              420

            Two ->
              578

            Three ->
              736

            Four ->
              893

            Five ->
              1050

            Six ->
              1207

            Seven ->
              1365

            Eight ->
              1680

        scale_ number =
          number * 1.15 ^ (toFloat (ilvl - 835) / 15)

      in
        round <| scale_ baseReduction
    _ ->
      0

scanForPromises : List WCL.Item -> Maybe Int
scanForPromises gear =
  Maybe.map .itemLevel <| find ((==) 128710 << .id) gear

getPromisesBuff : Int -> Maybe Promises
getPromisesBuff abilityID =
  case abilityID of
    191615 ->
      Just Ace

    191616 ->
      Just Two

    191617 ->
      Just Three

    191618 ->
      Just Four

    191619 ->
      Just Five

    191620 ->
     Just Six

    191621 ->
      Just Seven

    191622 ->
      Just Eight

    _ ->
      Nothing

onlyIf : Bool -> (a -> a) -> a -> a
onlyIf cond f =
  if cond then f else identity

usingDruid : CharacterID -> (Druid -> Druid) -> (Dict CharacterID Druid) -> (Dict CharacterID Druid)
usingDruid characterID f =
  Dict.update characterID (Maybe.map f)

bonusHealing : Model -> CharacterID -> Int
bonusHealing (Model druids) characterID =
  Maybe.withDefault 0 <| Maybe.map .usedSavings <| Dict.get characterID druids
