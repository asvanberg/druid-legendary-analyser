module Legendaries exposing (Legendary, Legendary(..), Model, all, itemId, init, update, bonusHealing)

import Legendaries.Boots as Boots
import Legendaries.Shoulders as Shoulders
import Legendaries.Wrists as Wrists
import Legendaries.Tearstone as Tearstone
import Legendaries.Waist as Waist

import WarcraftLogs.Models as WCL

type Model = Model
  { boots : Boots.Model
  , shoulders : Shoulders.Model
  , wrists : Wrists.Model
  , tearstone : Tearstone.Model
  , waist : Waist.Model
  }

type Legendary = Shoulders | Wrists | Boots | Tearstone | Waist
all : List Legendary
all = [ Shoulders, Wrists, Boots, Tearstone, Waist ]

itemId : Legendary -> Int
itemId legendary =
  case legendary of
    Shoulders -> 137072
    Wrists -> 137095
    Boots -> 137026
    Tearstone -> 137042
    Waist -> 137078

init : Model
init = Model
  { boots = Boots.init
  , shoulders = Shoulders.init
  , wrists = Wrists.init
  , tearstone = Tearstone.init
  , waist = Waist.init
  }

update : List WCL.Event -> Model -> Model
update events (Model model) =
  let
    newBoots = List.foldl Boots.parse model.boots events
    newShoulders = List.foldl Shoulders.parse model.shoulders events
    newWrists = List.foldl Wrists.parse model.wrists events
    newTearstone = List.foldl Tearstone.parse model.tearstone events
    newWaist = List.foldl Waist.parse model.waist events
  in
    Model
      { model
      | boots = newBoots
      , shoulders = newShoulders
      , wrists = newWrists
      , tearstone = newTearstone
      , waist = newWaist
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
        Tearstone ->
          Tearstone.bonusHealing model.tearstone
        Waist ->
          Waist.bonusHealing model.waist
  in
    legendaryBonushealing sourceID
