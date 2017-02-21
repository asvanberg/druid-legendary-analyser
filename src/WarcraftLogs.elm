module WarcraftLogs exposing (ApiKey, getFights, getEvents)

import Http
import Json.Decode exposing (..)
import Time exposing (Time)

import WarcraftLogs.Models exposing (..)

type alias ReportCode = String
type alias ApiKey = String

-- Does it work using one for everyone?
-- Are they meant for tools or personal use?
defaultApiKey : ApiKey
defaultApiKey = "087783eb78c21061d028831d2344d118"

-- The random number is used to circumvent browser caching if you're following
-- a live log and want new fights to show up.
getFights : Maybe ApiKey -> ReportCode -> Int -> Http.Request (List Fight, List Friendly)
getFights maybeApiKey reportCode randomNumber =
  let
    apiKey = Maybe.withDefault defaultApiKey maybeApiKey
    url = "https://www.warcraftlogs.com/v1/report/fights/" ++ reportCode
      ++ "?api_key=" ++ apiKey
      ++ "&" ++ (toString randomNumber)
    fights = field "fights" <| list fightDecoder
    friendlies = field "friendlies" <| list friendlyDecoder
  in
    Http.get url (map2 (,) fights friendlies)

getEvents : Maybe ApiKey -> ReportCode -> Time -> Time -> Http.Request EventPage
getEvents maybeApiKey reportCode start end =
  let
    apiKey = Maybe.withDefault defaultApiKey maybeApiKey
    url = "https://www.warcraftlogs.com/v1/report/events/" ++ reportCode
      ++ "?api_key=" ++ apiKey
      ++ "&start=" ++ toString start
      ++ "&end=" ++ toString end
  in
    Http.get url eventPage

fightDecoder : Decoder Fight
fightDecoder =
  map8 Fight
    (field "id" int)
    (field "name" string)
    (map (Maybe.withDefault False) <| maybe <| field "kill" bool)
    (field "start_time" float)
    (field "end_time" float)
    (field "boss" int)
    (maybe <| field "difficulty" int)
    (maybe <| field "bossPercentage" int)

friendlyDecoder : Decoder Friendly
friendlyDecoder =
  map4 Friendly
    (field "id" int)
    (field "name" string)
    (field "type" string)
    (field "fights" <| list <| field "id" int)

eventPage : Decoder EventPage
eventPage =
  map2 EventPage
    (field "events" (list event))
    (maybe <| field "nextPageTimestamp" float)

event : Decoder Event
event =
  field "type" string |> andThen (\t -> oneOf [ eventHelp t, succeed Unknown ])

eventHelp : String -> Decoder Event
eventHelp type_ =
  case type_ of
    "refreshdebuff" ->
      map RefreshDebuff targetedAbility
    "applydebuffstack" ->
      map ApplyDebuffStack targetedAbilityStack
    "applybuffstack" ->
      map ApplyBuffStack targetedAbilityStack
    "applydebuff" ->
      map ApplyDebuff targetedAbility
    "removebuffstack" ->
      map RemoveBuffStack targetedAbilityStack
    "removedebuff" ->
      map RemoveDebuff targetedAbility
    "removebuff" ->
      map RemoveBuff targetedAbility
    "applybuff" ->
      map ApplyBuff targetedAbility
    "damage" ->
      map Damage targetedAbilityAmount
    "energize" ->
      energize
    "cast" ->
      cast
    "heal" ->
      heal
    "absorbed" ->
      map Absorbed targetedAbilityAmount
    "summon" ->
      map Summon sourcedAbility
    "begincast" ->
      map BeginCast sourcedAbility
    "refreshbuff" ->
      map RefreshBuff targetedAbility
    "combatantinfo" ->
      combatantinfo
    "encounterstart" ->
      encounterstart
    _ -> fail <| "Unknown event type: '" ++ type_ ++ "'"

ability : Decoder Ability
ability =
  map2 Ability
    (field "guid" int)
    (field "name" string)

sourcedAbility : Decoder SourcedAbility
sourcedAbility =
  map3 SourcedAbility
    (field "timestamp" float)
    (field "sourceID" int)
    (field "ability" ability)

targetedAbility : Decoder TargetedAbility
targetedAbility =
  map4 TargetedAbility
    (field "timestamp" float)
    (field "sourceID" int)
    (field "targetID" int)
    (field "ability" ability)

targetedAbilityStack : Decoder TargetedAbilityStack
targetedAbilityStack =
  map5 TargetedAbilityStack
    (field "timestamp" float)
    (field "sourceID" int)
    (field "targetID" int)
    (field "ability" ability)
    (field "stack" int)

targetedAbilityAmount : Decoder TargetedAbilityAmount
targetedAbilityAmount =
  map5 TargetedAbilityAmount
    (field "timestamp" float)
    (field "sourceID" int)
    (field "targetID" int)
    (field "ability" ability)
    (field "amount" int)

energize : Decoder Event
energize =
  map6 (\timestamp sourceID targetID ability amount resourceType -> Energize
    { timestamp = timestamp
    , sourceID = sourceID
    , targetID = targetID
    , ability = ability
    , amount = amount
    , resourceType = resourceType
    })
    (field "timestamp" float)
    (field "sourceID" int)
    (field "targetID" int)
    (field "ability" ability)
    (field "resourceChange" int)
    (field "resourceChangeType" int)

cast : Decoder Event
cast =
  map4 (\timestamp sourceID targetID ability -> Cast
    { timestamp = timestamp
    , sourceID = sourceID
    , targetID = targetID
    , ability = ability
    })
    (field "timestamp" float)
    (field "sourceID" int)
    (maybe <| field "targetID" int)
    (field "ability" ability)

heal : Decoder Event
heal =
  let
    part1 = targetedAbilityAmount
    part2 =
      map4 (\overheal hitPoints maxHitPoints hitType ->
        { overheal = overheal
        , hitPoints = hitPoints
        , maxHitPoints = maxHitPoints
        , hitType = hitType
        })
        (map (Maybe.withDefault 0) <| maybe <| field "overheal" int)
        (field "hitPoints" int)
        (field "maxHitPoints" int)
        (field "hitType" int)
  in
    map2 (\{timestamp, sourceID, targetID, ability ,amount} {overheal, hitType, hitPoints, maxHitPoints} ->
      Heal
        { timestamp = timestamp
        , sourceID = sourceID
        , targetID = targetID
        , ability = ability
        , amount = amount
        , overheal = overheal
        , hitPoints = hitPoints
        , maxHitPoints = maxHitPoints
        , hitType = hitType
        }
      )
      part1
      part2

combatantinfo : Decoder Event
combatantinfo =
  map4 (\sourceID specID artifact gear -> CombatantInfo
    { sourceID = sourceID
    , specID = specID
    , artifact = artifact
    , gear = gear
    })
    (field "sourceID" int)
    (field "specID" int)
    (field "artifact" (list trait))
    (field "gear" (list item))

trait : Decoder Trait
trait =
  map3 Trait
    (field "traitID" int)
    (field "spellID" int)
    (field "rank" int)

item : Decoder Item
item =
  map Item
    (field "id" int)

encounterstart : Decoder Event
encounterstart =
  map5 (\timestamp name size difficulty encounterID -> EncounterStart
    { timestamp = timestamp
    , name = name
    , size = size
    , difficulty = difficulty
    , encounterID = encounterID
    })
    (field "timestamp" float)
    (field "name" string)
    (field "size" int)
    (field "difficulty" int)
    (field "encounterID" int)

type alias SourcedAbility =
  { timestamp : Time
  , sourceID : Int
  , ability : Ability
  }

type alias TargetedAbility =
  { timestamp : Time
  , sourceID : Int
  , targetID : Int
  , ability : Ability
  }

type alias TargetedAbilityStack =
  { timestamp : Time
  , sourceID : Int
  , targetID : Int
  , ability : Ability
  -- Stacks _LEFT AFTER_ removal. Goes down to 1 then RemoveBuff
  -- Stacks _AFTER_ adding new ones. ApplyBuff on first
  , stacks : Int
  }

type alias TargetedAbilityAmount =
  { timestamp : Time
  , sourceID : Int
  , targetID : Int
  , ability : Ability
  , amount : Int
  }
