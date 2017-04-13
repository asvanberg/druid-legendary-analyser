module Legendaries exposing (ItemID, ItemID(..), Legendary, Legendary(..), Model, all, itemId, init, update, bonusHealing)

import Analyser.Boots as Boots
import Analyser.Chest as Chest
import Analyser.Wrists as Wrists
import Analyser.Tearstone as Tearstone
import Analyser.Waist as Waist
import Analyser.Drape as Drape
import Analyser.Trinket as Trinket
import Analyser.Tier19 as Tier19
import Analyser.Rejuvenation as Rejuvenation

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

type Legendary
  = Wrists
  | Boots
  | Tearstone
  | Waist
  | Chest
  | Drape
  | Trinket
  | Tier19
  | Shoulders
  | DeepRooted

all : List Legendary
all =
  [ Wrists
  , Boots
  , Tearstone
  , Waist
  , Chest
  , Drape
  , Trinket
  , Tier19
  , Shoulders
  , DeepRooted
  ]

type ItemID
  = Item Int
  | Set Int Int (List Int)
  | Trait Int

itemId : Legendary -> ItemID
itemId legendary =
  case legendary of
    Wrists -> Item 137095
    Boots -> Item 137026
    Tearstone -> Item 137042
    Waist -> Item 137078
    Chest -> Item 137015
    Drape -> Item 142170
    Trinket -> Item 144258
    Tier19 -> Set 1283 4 [ 138324, 138327, 138330, 138333, 138336, 138366 ]
    Shoulders -> Item 137072
    DeepRooted -> Trait 238122

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

bonusHealing : Legendary -> Model -> Int -> Int
bonusHealing legendary (Model model) sourceID =
  let
    legendaryBonushealing =
      case legendary of
        Boots ->
          Boots.bonusHealing model.boots
        Wrists ->
          Wrists.bonusHealing model.wrists
        Tearstone ->
          Tearstone.bonusHealing model.tearstone
        Waist ->
          Waist.bonusHealing model.waist
        Chest ->
          Chest.bonusHealing model.chest
        Drape ->
          Drape.bonusHealing model.drape
        Trinket ->
          Trinket.bonusHealing model.trinket
        Tier19 ->
          Tier19.bonusHealing model.tier19
        Shoulders ->
          Rejuvenation.bonusHealing model.rejuvenation Rejuvenation.Shoulders
        DeepRooted ->
          Rejuvenation.bonusHealing model.rejuvenation Rejuvenation.DeepRootedTrait
  in
    legendaryBonushealing sourceID
