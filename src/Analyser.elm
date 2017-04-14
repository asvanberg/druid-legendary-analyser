module Analyser exposing (..)

import Analyser.Boots as Boots
import Analyser.Chest as Chest
import Analyser.Wrists as Wrists
import Analyser.Tearstone as Tearstone
import Analyser.Waist as Waist
import Analyser.Drape as Drape
import Analyser.Trinket as Trinket
import Analyser.Tier19 as Tier19
import Analyser.Rejuvenation as Rejuvenation

import Legendaries exposing (Legendary(..), BonusHealing(..), Source(..))

import WarcraftLogs.Models as WCL

type Model = Model
  { boots : Boots.Model
  , wrists : Wrists.Model
  , tearstone : Tearstone.Model
  , waist : Waist.Model
  , chest : Chest.Model
  , drape : Drape.Model
  , trinket : Trinket.Model
  , tier19 : Tier19.Model
  , rejuvenation : Rejuvenation.Model
  }

init : Model
init = Model
  { boots = Boots.init
  , wrists = Wrists.init
  , tearstone = Tearstone.init
  , waist = Waist.init
  , chest = Chest.init
  , drape = Drape.init
  , trinket = Trinket.init
  , tier19 = Tier19.init
  , rejuvenation = Rejuvenation.init
  }

update : List WCL.Event -> Model -> Model
update events (Model model) =
  let
    newBoots = List.foldl Boots.parse model.boots events
    newWrists = List.foldl Wrists.parse model.wrists events
    newTearstone = List.foldl Tearstone.parse model.tearstone events
    newWaist = List.foldl Waist.parse model.waist events
    newChest = List.foldl Chest.parse model.chest events
    newDrape = List.foldl Drape.parse model.drape events
    newTrinket = List.foldl Trinket.parse model.trinket events
    newTier19 = List.foldl Tier19.parse model.tier19 events
    newRejuvenation = List.foldl Rejuvenation.parse model.rejuvenation events
  in
    Model
      { model
      | boots = newBoots
      , wrists = newWrists
      , tearstone = newTearstone
      , waist = newWaist
      , chest = newChest
      , drape = newDrape
      , trinket = newTrinket
      , tier19 = newTier19
      , rejuvenation = newRejuvenation
      }

bonusHealing : Legendary -> Model -> Int -> BonusHealing
bonusHealing legendary (Model model) sourceID =
  let
    legendaryBonushealing =
      case legendary of
        Boots ->
          Simple << Boots.bonusHealing model.boots
        Wrists ->
          Simple << Wrists.bonusHealing model.wrists
        Tearstone ->
          Tearstone.bonusHealing model.tearstone
        Waist ->
          Simple << Waist.bonusHealing model.waist
        Chest ->
          Simple << Chest.bonusHealing model.chest
        Drape ->
          Simple << Drape.bonusHealing model.drape
        Trinket ->
          Simple << Trinket.bonusHealing model.trinket
        Tier19 ->
          Tier19.bonusHealing model.tier19
        Shoulders ->
          Rejuvenation.bonusHealing model.rejuvenation Shoulders
        DeepRooted ->
          Rejuvenation.bonusHealing model.rejuvenation DeepRooted
  in
    legendaryBonushealing sourceID
