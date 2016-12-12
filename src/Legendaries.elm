module Legendaries exposing (Legendary, Legendary(..), Model, all, itemId, init, update, bonusHealing)

import Legendaries.Boots as Boots
import Legendaries.Shoulders as Shoulders
import Legendaries.Wrists as Wrists

import WarcraftLogs.Models as WCL

type Model = Model
  { boots : Boots.Model
  , shoulders : Shoulders.Model
  , wrists : Wrists.Model
  }

type Legendary = Shoulders | Wrists | Boots
all : List Legendary
all = [ Shoulders, Wrists, Boots ]

itemId : Legendary -> Int
itemId legendary =
  case legendary of
    Shoulders -> 137072
    Wrists -> 137095
    Boots -> 137026

init : Model
init = Model
  { boots = Boots.init
  , shoulders = Shoulders.init
  , wrists = Wrists.init
  }

update : List WCL.Event -> Model -> Model
update events (Model model) =
  let
    newBoots = List.foldl Boots.parse model.boots events
    newShoulders = List.foldl Shoulders.parse model.shoulders events
    newWrists = List.foldl Wrists.parse model.wrists events
  in
    Model
      { model
      | boots = newBoots
      , shoulders = newShoulders
      , wrists = newWrists
      }

bonusHealing : Legendary -> Model -> Int -> Int
bonusHealing legendary (Model model) sourceID =
  let
    legendaryBonushealing =
      case legendary of
        Boots ->
          Boots.bonusHealing model.boots
        Shoulders ->
          Shoulders.bonusHealing model.shoulders
        Wrists ->
          Wrists.bonusHealing model.wrists
  in
    legendaryBonushealing sourceID
