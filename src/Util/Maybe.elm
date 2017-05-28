module Util.Maybe exposing ((?), orElse)

(?) : Maybe a -> a -> a
(?) = flip Maybe.withDefault

orElse : Maybe a -> Maybe a -> Maybe a
orElse m2 =
  Maybe.withDefault m2 << Maybe.map Just
