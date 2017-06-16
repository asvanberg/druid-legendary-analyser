module View exposing (view)

import Dict
import GenericDict exposing (GenericDict)
import Html exposing (..)
import Html.Attributes exposing (autofocus, class, classList, href, placeholder, style, type_, value)
import Html.Events exposing (defaultOptions, onClick, onInput, onWithOptions)
import Json.Decode as Decode
import Regex as R

import Analyser
import Legendaries exposing (Legendary(..), Source(..), BonusHealing(..), BonusType(..))
import Model exposing (..)

import WarcraftLogs.Models as WCL

view : Model -> Html Message
view model =
  div [] <|
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
        , input [ autofocus <| String.isEmpty model.reportCode, class "form-control", placeholder "Enter report code", onInput EnteringReportCode, value model.reportCode ] []
        ]
      , button [ class "btn btn-default", onClick <| GetFights Nothing Nothing, type_ "button" ] [ text "Get fights" ]
      , div
          [ classList [ ("form-group", True), ("hidden", List.isEmpty model.fights) ] ]
          [ viewFights model ]
      ]
    , case model.selectedFight of
        Just fight ->
          div []
            [ h2 [] [ text <| fightTitle model.fights fight ]
            , div [ class "progress" ]
              [ div
                  [ class "progress-bar"
                  , style [ ("width", (toString <| model.processed * 100) ++ "%") ]
                  ]
                  [ text <| toString <| round <| model.processed * 100, text "%" ]
              ]
            , div [ class "row" ] <| List.map (viewDruid model) <| Dict.values model.druids
            ]
        Nothing ->
          div [] []
    ]

viewFights : Model -> Html Message
viewFights model =
  div [ classList [ ("dropdown", True), ("open", model.fightSelectionOpen) ] ]
    [ button [ class "btn btn-default dropdown-toggle", type_ "button", onClick OpenFightSelection ]
      [ text "Select fight "
      , span [ class "caret"] []
      ]
    , ul [ class "dropdown-menu" ]
        <| List.map (viewFight model.reportCode model.fights) model.fights
    ]

viewFight : String -> List WCL.Fight -> WCL.Fight -> Html Message
viewFight reportCode fights fight =
  let
    preventDefault =
      { defaultOptions | preventDefault = True }
    onClick_ msg =
      onWithOptions "click" preventDefault (Decode.succeed msg)
    href_ =
      "?reportCode=" ++ reportCode ++ "&fight=" ++ (toString fight.id)
  in
    li []
      [ a [ onClick_ <| SelectFight fight, href href_ ]
        [ text <| fightTitle fights fight
        ]
      ]

fightTitle : List WCL.Fight -> WCL.Fight -> String
fightTitle fights fight =
  let
    sameEncounter fight1 fight2 =
      fight1.boss == fight2.boss && fight1.difficulty == fight2.difficulty
    wipeNumber = List.filter (sameEncounter fight) fights
      |> List.filter (\f -> f.id < fight.id)
      |> List.length
      |> (+) 1
    zeroPad n = if n < 10 then "0" ++ (toString n) else toString n
    duration = floor <| (fight.end - fight.start) / 1000
    minutes = duration // 60
    seconds = duration % 60
    durationString = (toString minutes) ++ ":" ++ (zeroPad seconds)
    difficultyName =
      case fight.difficulty of
        Just 5 -> "Mythic"
        Just 4 -> "Heroic"
        Just 3 -> "Normal"
        Just 1 -> "LFR"
        _ -> ""
  in
    difficultyName
      ++ " "
      ++ fight.name
      ++ " "
      ++ (if fight.kill then "kill" else "wipe #" ++ (toString wipeNumber))
      ++ " "
      ++ "(" ++ durationString ++ ")"
      ++ case (fight.kill, fight.bossPercentage) of
        (False, Just bossPercentage) -> " - " ++ (toString <| bossPercentage // 100) ++ "%"
        _ -> ""

viewDruid : Model -> Druid -> Html Message
viewDruid model druid =
  div [ class "col-lg-6 col-md-12 col-sm-6" ]
    [ div [ class "panel panel-default" ]
      [ div [class "panel-heading"] [ text druid.name ]
      , ul [ class "list-group" ]
        <| List.concatMap (viewLegendary model druid) druid.legendaries
        ++ [ li [ class "list-group-item" ] [ text "Total", span [ class "pull-right" ] [ text <| thousandSep druid.healingDone ] ] ]
      ]
    ]

viewLegendary : Model -> Druid -> Legendary -> List (Html Message)
viewLegendary model druid legendary =
  let
    bonusHealing = Analyser.bonusHealing legendary model.legendaries druid.id
    percentage amount =
       toFloat (amount * 10000 // druid.healingDone) / 100 -- Rounding to 2 digits
    legendaryName =
      case legendary of
        Boots ->
          "Boots"
        Wrists ->
          "Wrists"
        Tearstone ->
          "Tearstone"
        Waist ->
          "Waist"
        Chest ->
          "Chest"
        Drape ->
          "Drape of Shame"
        Trinket ->
          "Velen's Future Sight"
        Tier19 ->
          "Garb of the Astral Warden"
        Shoulders ->
          "Shoulders"
        DeepRooted ->
          "Deep Rooted"
        Tier20 ->
          "Tier 20 4pc"
        Soul ->
          "Soul of the Archdruid"
        Promises ->
          "Darkmoon Deck: Promises"

    wowheadLink itemId =
      case itemId of
        Legendaries.Item id ->
          "http://www.wowhead.com/item=" ++ (toString id)
        Legendaries.Set id _ _ ->
          "http://www.wowhead.com/item-set=" ++ (toString id)
        Legendaries.Trait id ->
          "http://www.wowhead.com/spell=" ++ (toString id)
        Legendaries.Aura id ->
          "http://www.wowhead.com/spell=" ++ (toString id)

    totalItem amount =
      li [ class "list-group-item" ]
        [ a [ href <| wowheadLink <| Legendaries.itemId legendary ] [ strong [] [ text legendaryName ] ]
        , span [ class "pull-right" ] [ text <| thousandSep amount, text " (", text <| toString <| percentage amount, text "%)" ]
        ]

    manaItem amount =
      li [ class "list-group-item" ]
        [ a [ href <| wowheadLink <| Legendaries.itemId legendary ] [ strong [] [ text legendaryName ] ]
        , span [ class "pull-right text-info" ] [ text <| thousandSep amount, text " extra mana" ]
        ]

    showSource (source, amount) =
      li [ class "list-group-item small" ]
        [ span [ class "col-xs-offset-1" ] [ text (sourceName source) ]
        , span [ class "pull-right" ] [ text (thousandSep amount) ]
        ]
  in
    case bonusHealing of
      Simple Healing amount ->
        [ totalItem amount ]

      Simple Mana amount ->
        [ manaItem amount ]

      Breakdown sources ->
        totalItem (calculateTotal sources) :: (GenericDict.toList sources |> List.sortBy Tuple.second |> List.reverse |> List.map showSource)

thousandSep : Int -> String
thousandSep = R.replace R.All (R.regex "\\B(?=(\\d{3})+(?!\\d))") (always ",") << toString

calculateTotal : GenericDict Source Int -> Int
calculateTotal =
  GenericDict.foldl (always (+)) 0

sourceName : Source -> String
sourceName source =
  case source of
    Rejuvenation ->
      "Rejuvenation"

    Dreamwalker ->
      "Dreamwalker"

    WildGrowth ->
      "Wild Growth"

    Increase ->
      "Healing increase"

    Overheal ->
      "Overheal spread"
