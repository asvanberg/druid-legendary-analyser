module Main exposing (main)

import Dict exposing (Dict)
import Http
import Navigation
import Random
import Time exposing (second)

import Legendaries exposing (Legendary, Legendary(..))
import Model exposing (..)
import View exposing (view)

import Util.List exposing (find)

import WarcraftLogs
import WarcraftLogs.Models as WCL

main : Program Never Model Message
main = Navigation.program UrlChange
  { init = init
  , update = update
  , view = view
  , subscriptions = subscriptions
  }

init : Navigation.Location -> (Model, Cmd Message)
init location =
  let model =
    { reportCode = ""
    , fights = []
    , friendlies = []
    , legendaries = Legendaries.init
    , processed = 0
    , errorMessage = Nothing
    , druids = Dict.empty
    , fightSelectionOpen = False
    , selectedFight = Nothing
    }
  in update (UrlChange location) model

subscriptions : Model -> Sub Message
subscriptions model =
  case model.errorMessage of
    Just _ ->
      Time.every (5 * second) (always ClearErrorMessage)

    Nothing ->
      Sub.none

update : Message -> Model -> (Model, Cmd Message)
update msg model = case msg of
  UrlChange location ->
    let
      toKeyValuePair segment =
        case String.split "=" segment of
          [key, value] ->
            Maybe.map2 (,) (Http.decodeUri key) (Http.decodeUri value)

          _ ->
            Nothing

      queryParams = location.search
        |> String.dropLeft 1
        |> String.split "&"
        |> List.filterMap toKeyValuePair
        |> Dict.fromList

      maybeReportCode = Dict.get "reportCode" queryParams
      maybeFight = Dict.get "fight" queryParams
        |> Maybe.andThen (Result.toMaybe << String.toInt)
    in
      case maybeReportCode of
        Just reportCode ->
          update (GetFights maybeFight Nothing) { model | reportCode = reportCode }

        Nothing ->
          (model, Cmd.none)

  EnteringReportCode reportCode ->
    let newModel = { model | reportCode = reportCode }
    in (newModel, Cmd.none)

  ClearErrorMessage ->
    ({ model | errorMessage = Nothing }, Cmd.none)

  GetFights toAnalyze (Just randomNumber)  ->
    let
      cmd = Http.send (FightsRetrieved toAnalyze)
        <| WarcraftLogs.getFights model.reportCode randomNumber
    in (model, cmd)

  GetFights toAnalyze Nothing ->
    let
      randomNumber = Random.int Random.minInt Random.maxInt
      cmd = Random.generate ((GetFights toAnalyze) << Just) randomNumber
    in
      (model, cmd)

  FightsRetrieved _ (Err _) ->
    ({ model | errorMessage = Just "Failed to fetch fights in the given report" }, Cmd.none)

  FightsRetrieved toAnalyze (Ok (fights, friendlies)) ->
    let
      druids = List.filter ((==) "Druid" << .class) friendlies
      relevantFights = List.filter ((<) 0 << .boss) fights
      newModel = { model | fights = relevantFights, friendlies = druids }

      findCorrectFight fightID = find ((==) fightID << .id) relevantFights
      (finalModel, cmd) =
        case Maybe.andThen findCorrectFight toAnalyze of
          Just fight ->
            update (Analyze fight) newModel

          Nothing ->
            (newModel, Cmd.none)

    in (finalModel, cmd)

  Analyze fight ->
    let
      -- Start 30 seconds earlier to track pre-hotting
      start = fight.start - 30 * second
      request = WarcraftLogs.getEvents model.reportCode start fight.end
      cmd = Http.send (EventsRetrieved fight) request
      newModel =
        { model
        | legendaries = Legendaries.init
        , druids = Dict.empty
        , processed = 0
        , selectedFight = Just fight
        }
    in
      (newModel, cmd)

  SelectFight fight ->
    let
      updateUrl = Navigation.newUrl
        <| "?reportCode=" ++ model.reportCode ++ "&fight=" ++ (toString fight.id)
      newModel = { model | fightSelectionOpen = False }
    in
      (newModel, updateUrl)

  EventsRetrieved _ (Err err) ->
    ({ model | errorMessage = Just "Failed to fetch events in the selected fight" }, Cmd.none)

  EventsRetrieved fight (Ok page) ->
    let
      events = page.events

      newDruids = scanForDruids events model.friendlies model.druids
      newLegendaries = Legendaries.update events model.legendaries

      (processed, cmd) =
        case page.nextPageStartTimestamp of
          Just nextPageStartTimestamp ->
            let
              request = WarcraftLogs.getEvents model.reportCode nextPageStartTimestamp fight.end
              nextPage = Http.send (EventsRetrieved fight) request
            in ((nextPageStartTimestamp - fight.start) / (fight.end - fight.start), nextPage)

          Nothing ->
            (1, Cmd.none)

      newModel =
        { model
        | legendaries = newLegendaries
        , processed = processed
        , druids = newDruids
        }
    in
      (newModel, cmd)

  OpenFightSelection ->
    ({ model | fightSelectionOpen = not model.fightSelectionOpen }, Cmd.none)

scanForDruids : List WCL.Event -> List WCL.Friendly -> Dict Int Druid -> Dict Int Druid
scanForDruids events friendlies druids =
  let
    addHealingDone amount druid = { druid | healingDone = druid.healingDone + amount}
    scanForDruid event druids =
      case event of
        WCL.CombatantInfo {sourceID, specID, gear} ->
          if specID == 105 then
            let
              isEquipped legendary =
                List.any ((==) (Legendaries.itemId legendary) << .id) gear
              legendaries = List.filter isEquipped Legendaries.all
              name = find ((==) sourceID << .id) friendlies
                |> Maybe.map .name
                |> Maybe.withDefault "Unknown"
              druid = { id = sourceID
                      , name = name
                      , legendaries = legendaries
                      , healingDone = 0
                      }
            in
              Dict.insert sourceID druid druids
          else
            druids

        WCL.Heal {sourceID, amount} ->
          Dict.update sourceID (Maybe.map <| addHealingDone amount) druids

        WCL.Absorbed {sourceID, amount} ->
          Dict.update sourceID (Maybe.map <| addHealingDone amount) druids

        _ ->
          druids

  in
    List.foldl scanForDruid druids events
