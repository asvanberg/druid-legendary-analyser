module Legendaries exposing
  ( ItemID(..)
  , Legendary(..)
  , BonusHealing(..)
  , Source(..)
  , all
  , compareLegendary
  , itemId
  , compareSource
  )

import GenericDict exposing (GenericDict)

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
  | Tier20
  | Soul

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
  , Tier20
  , Soul
  ]

compareLegendary : Legendary -> Legendary -> Order
compareLegendary l1 l2 =
  compare (toString l1) (toString l2)

type ItemID
  = Item Int
  | Set Int Int (List Int)
  | Trait Int
  | Aura Int

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
    Tier20 -> Aura 242313
    Soul -> Item 151636

type BonusHealing
  = Simple Int
  | Breakdown (GenericDict Source Int)

type Source
  = Rejuvenation
  | Dreamwalker
  | WildGrowth

compareSource : Source -> Source -> Order
compareSource s1 s2 =
  compare (toString s1) (toString s2)
