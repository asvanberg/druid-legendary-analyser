module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (class, classList, href, placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Random
import Time exposing (second)

import Legendaries exposing (Legendary, Legendary(..))

import Util.List exposing (find)

import WarcraftLogs
import WarcraftLogs.Models as WCL

type alias Model =
  { reportCode : String
  , fights : List WCL.Fight
  , friendlies : List WCL.Friendly
  , legendaries : Legendaries.Model
  , processed : Float
  , errorMessage : Maybe String
  , druids : List Druid
  }

type alias Druid =
  { id : Int
  , name : String
  , legendaries : List Legendary
  }

type Message
  = EnteringReportCode String
  | GetFights (Maybe Int)
  | FightsRetrieved (Result Http.Error (List WCL.Fight, List WCL.Friendly))
  | Analyze WCL.Fight
  | EventsRetrieved WCL.Fight (Result Http.Error WCL.EventPage)
  | ClearErrorMessage

main : Program Never Model Message
main = program
  { init = init
  , update = update
  , view = view
  , subscriptions = subscriptions
  }

init : (Model, Cmd Message)
init =
  let model =
    { reportCode = ""
    , fights = []
    , friendlies = []
    , legendaries = Legendaries.init
    , processed = 0
    , errorMessage = Nothing
    , druids = []
    }
  in (model, Cmd.none)

subscriptions : Model -> Sub Message
subscriptions model =
  case model.errorMessage of
    Just _ ->
      Time.every (5 * second) (always ClearErrorMessage)

    Nothing ->
      Sub.none

update : Message -> Model -> (Model, Cmd Message)
update msg model = case msg of
  EnteringReportCode reportCode ->
    let newModel = { model | reportCode = reportCode }
    in (newModel, Cmd.none)

  ClearErrorMessage ->
    ({ model | errorMessage = Nothing }, Cmd.none)

  GetFights (Just randomNumber) ->
    let
      cmd = Http.send FightsRetrieved <| WarcraftLogs.getFights model.reportCode randomNumber
    in (model, cmd)

  GetFights Nothing ->
    let
      randomNumber = Random.int Random.minInt Random.maxInt
      cmd = Random.generate (GetFights << Just) randomNumber
    in
      (model, cmd)

  FightsRetrieved (Err _) ->
    ({ model | errorMessage = Just "Failed to fetch fights in the given report" }, Cmd.none)

  FightsRetrieved (Ok (fights, friendlies)) ->
    let
      druids = List.filter ((==) "Druid" << .class) friendlies
      relevantFights = List.filter ((<) 0 << .boss) fights
      newModel = { model | fights = relevantFights, friendlies = druids }
    in (newModel, Cmd.none)

  Analyze fight ->
    let
      -- Start 30 seconds earlier to track pre-hotting
      start = fight.start - 30 * second
      request = WarcraftLogs.getEvents model.reportCode start fight.end
      cmd = Http.send (EventsRetrieved fight) request
      newModel =
        { model
        | legendaries = Legendaries.init
        , druids = []
        , processed = 0
        }
    in
      (newModel, cmd)

  EventsRetrieved _ (Err err) ->
    ({ model | errorMessage = Just "Failed to fetch events in the selected fight" }, Cmd.none)

  EventsRetrieved fight (Ok page) ->
    let
      events = page.events

      newDruids = scanForDruids events model.friendlies
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
        , druids = model.druids ++ newDruids
        }
    in
      (newModel, cmd)

scanForDruids : List WCL.Event -> List WCL.Friendly -> List Druid
scanForDruids events friendlies =
  let
    scanForDruid event =
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
            in
              Just { id = sourceID
                   , name = name
                   , legendaries = legendaries
                   }
          else
            Nothing

        _ ->
          Nothing
  in
    List.filterMap scanForDruid events

view : Model -> Html Message
view model =
  div [ class "container" ]
    [ case model.errorMessage of
        Just errorMessage ->
          div [ class "alert alert-danger" ]
            [ button [ type_ "button", class "close", onClick ClearErrorMessage ] [ text "×" ]
            , text errorMessage
            ]

        Nothing ->
          div [] []
    , form [ class "form-inline" ]
      [ div [ class "form-group" ]
        [ label [ class "sr-only" ] [ text "Report code" ]
        , input [ class "form-control", placeholder "Enter report code", onInput EnteringReportCode ] []
        ]
      , button [ class "btn btn-default", onClick <| GetFights Nothing, type_ "button" ] [ text "Get fights" ]
      ]
    , div [ classList [ ("row", True), ("hidden", List.isEmpty model.fights) ] ]
      [ div [ class "col-md-3" ]
        [ text "Pick a fight"
        , ul [] <| List.map viewFight model.fights
        ]
      , div [ class "col-md-9" ]
        [ div [ class "progress" ]
          [ div [ class "progress-bar", style [ ("width", (toString <| round <| model.processed * 100) ++ "%") ] ]
            [ text <| toString <| round <| model.processed * 100, text "%" ]
          ]
        , div [] <| List.map (viewDruid model) model.druids
        ]
      ]
    ]

viewFight : WCL.Fight -> Html Message
viewFight fight =
  li []
    [ a [ onClick <| Analyze fight, href "#" ] -- Dummy href for browser styling
      [ text fight.name
      , text <| if fight.kill then " kill" else " wipe"
      ]
    ]

viewDruid : Model -> Druid -> Html Message
viewDruid model druid =
  div []
    [ text druid.name
    , ul [] <| List.map (viewLegendary model druid.id) druid.legendaries
    ]

viewLegendary : Model -> Int -> Legendary -> Html Message
viewLegendary model druidID legendary =
  let
    bonusHealing =
      toString <| Legendaries.bonusHealing legendary model.legendaries druidID
    legendaryName =
      case legendary of
        Shoulders ->
          "Shoulders"

        Boots ->
          "Boots"

        Wrists ->
          "Wrists"

        Tearstone ->
          "Tearstone"
  in
    li [] [ text legendaryName, text ": ", text bonusHealing ]
