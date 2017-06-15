module Analyser exposing (..)

import Analyser.Boots as Boots
import Analyser.Chest as Chest
import Analyser.Wrists as Wrists
import Analyser.Waist as Waist
import Analyser.Drape as Drape
import Analyser.Trinket as Trinket
import Analyser.Rejuvenation as Rejuvenation
import Analyser.Tier20 as Tier20
import Analyser.Soul as Soul
import Analyser.Promises as Promises

import Legendaries exposing (Legendary(..), BonusHealing(..), BonusType(..), Source(..))

import WarcraftLogs.Models as WCL

type Model = Model
  { boots : Boots.Model
  , wrists : Wrists.Model
  , waist : Waist.Model
  , chest : Chest.Model
  , drape : Drape.Model
  , trinket : Trinket.Model
  , rejuvenation : Rejuvenation.Model
  , tier20 : Tier20.Model
  , soul : Soul.Model
  , promises : Promises.Model
  }

init : Model
init = Model
  { boots = Boots.init
  , wrists = Wrists.init
  , waist = Waist.init
  , chest = Chest.init
  , drape = Drape.init
  , trinket = Trinket.init
  , rejuvenation = Rejuvenation.init
  , tier20 = Tier20.init
  , soul = Soul.init
  , promises = Promises.init
  }

update : List WCL.Event -> Model -> Model
update events (Model model) =
  let
    newBoots = List.foldl Boots.parse model.boots events
    newWrists = List.foldl Wrists.parse model.wrists events
    newWaist = List.foldl Waist.parse model.waist events
    newChest = List.foldl Chest.parse model.chest events
    newDrape = List.foldl Drape.parse model.drape events
    newTrinket = List.foldl Trinket.parse model.trinket events
    newRejuvenation = List.foldl Rejuvenation.parse model.rejuvenation events
    newTier20 = List.foldl Tier20.parse model.tier20 events
    newSoul = List.foldl Soul.parse model.soul events
    newPromises = List.foldl Promises.parse model.promises events
  in
    Model
      { model
      | boots = newBoots
      , wrists = newWrists
      , waist = newWaist
      , chest = newChest
      , drape = newDrape
      , trinket = newTrinket
      , rejuvenation = newRejuvenation
      , tier20 = newTier20
      , soul = newSoul
      , promises = newPromises
      }

bonusHealing : Legendary -> Model -> Int -> BonusHealing
bonusHealing legendary (Model model) sourceID =
  let
    legendaryBonushealing =
      case legendary of
        Boots ->
          Simple Healing << Boots.bonusHealing model.boots
        Wrists ->
          Simple Healing << Wrists.bonusHealing model.wrists
        Tearstone ->
          Rejuvenation.bonusHealing model.rejuvenation Tearstone
        Waist ->
          Simple Healing << Waist.bonusHealing model.waist
        Chest ->
          Simple Healing << Chest.bonusHealing model.chest
        Drape ->
          Simple Healing << Drape.bonusHealing model.drape
        Trinket ->
          Trinket.bonusHealing model.trinket
        Tier19 ->
          Rejuvenation.bonusHealing model.rejuvenation Tier19
        Shoulders ->
          Rejuvenation.bonusHealing model.rejuvenation Shoulders
        DeepRooted ->
          Rejuvenation.bonusHealing model.rejuvenation DeepRooted
        Tier20 ->
          Simple Healing << Tier20.bonusHealing model.tier20
        Soul ->
          Soul.bonusHealing model.soul
        Promises ->
          Simple Mana << Promises.bonusHealing model.promises
  in
    legendaryBonushealing sourceID
