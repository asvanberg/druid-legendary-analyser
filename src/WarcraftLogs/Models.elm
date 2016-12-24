module WarcraftLogs.Models exposing (..)

import Time exposing (Time)

type alias Fight =
  { id : Int
  , name : String
  , kill : Bool
  , start : Time
  , end : Time
  , boss : Int
  , difficulty : Maybe Int
  }

type alias Friendly =
  { id : Int
  , name : String
  , class : String
  , fights : List Int
  }

type alias EventPage =
  { events : List Event
  , nextPageStartTimestamp : Maybe Time
  }

type Event
  = EncounterStart
    { timestamp : Time
    , name : String
    , size : Int
    , difficulty : Int
    , encounterID : Int
    }
  | CombatantInfo
    { sourceID : Int
    , specID : Int -- See Blizzard_TalentUI.lua
    , artifact : List Trait
    , gear : List Item
    }

  | BeginCast
    { timestamp : Time
    , sourceID : Int
    , ability : Ability
    }
  | Cast
    { timestamp : Time
    , sourceID : Int
    , targetID : Maybe Int -- Some spells like Tranquility requires no target
    , ability : Ability
    }

  | Heal
    { timestamp : Time
    , sourceID : Int
    , targetID : Int
    , ability : Ability
    , amount : Int -- Does not include overheal
    , overheal : Int
    , hitPoints : Int -- Target hit points _AFTER_ the heal landed
    , maxHitPoints : Int -- target max hit points
    , hitType : Int
    }
  | Absorbed
    { timestamp : Time
    , sourceID : Int
    , targetID : Int
    , ability : Ability
    , amount : Int
    }
  | Damage
    { timestamp : Time
    , sourceID : Int
    , targetID : Int
    , ability : Ability
    , amount : Int
    }
  | Energize
    { timestamp : Time
    , sourceID : Int
    , targetID : Int
    , ability : Ability
    , amount : Int -- "resourceChange"
    , resourceType : Int -- "resourceChangeType" see Blizzard Constants.lua
    }

  | ApplyBuff
    { timestamp : Time
    , sourceID : Int
    , targetID : Int
    , ability : Ability
    }
  | ApplyBuffStack
    { timestamp : Time
    , sourceID : Int
    , targetID : Int
    , ability : Ability
    -- Stacks _LEFT AFTER_ removal. Goes down to 1 then RemoveBuff
    -- Stacks _AFTER_ adding new ones. ApplyBuff on first
    , stacks : Int
    }
  | RefreshBuff
    { timestamp : Time
    , sourceID : Int
    , targetID : Int
    , ability : Ability
    }
  | RemoveBuffStack
    { timestamp : Time
    , sourceID : Int
    , targetID : Int
    , ability : Ability
    -- Stacks _LEFT AFTER_ removal. Goes down to 1 then RemoveBuff
    -- Stacks _AFTER_ adding new ones. ApplyBuff on first
    , stacks : Int
    }
  | RemoveBuff
    { timestamp : Time
    , sourceID : Int
    , targetID : Int
    , ability : Ability
    }

  | ApplyDebuff
    { timestamp : Time
    , sourceID : Int
    , targetID : Int
    , ability : Ability
    }
  | ApplyDebuffStack
    { timestamp : Time
    , sourceID : Int
    , targetID : Int
    , ability : Ability
    -- Stacks _LEFT AFTER_ removal. Goes down to 1 then RemoveBuff
    -- Stacks _AFTER_ adding new ones. ApplyBuff on first
    , stacks : Int
    }
  | RefreshDebuff
    { timestamp : Time
    , sourceID : Int
    , targetID : Int
    , ability : Ability
    }
  | RemoveDebuffStack
    { timestamp : Time
    , sourceID : Int
    , targetID : Int
    , ability : Ability
    -- Stacks _LEFT AFTER_ removal. Goes down to 1 then RemoveBuff
    -- Stacks _AFTER_ adding new ones. ApplyBuff on first
    , stacks : Int
    }
  | RemoveDebuff
    { timestamp : Time
    , sourceID : Int
    , targetID : Int
    , ability : Ability
    }

  | Summon
    { timestamp : Time
    , sourceID : Int
    , ability : Ability
    }

  | Unknown

type alias Trait =
  { traitID : Int
  , spellID : Int
  , rank : Int
  }

type alias Item =
  { id : Int
  }

type alias Ability =
  { id : Int
  , name : String
  --, school : Int -- bitset see Blizzard Constants.lua
  }
