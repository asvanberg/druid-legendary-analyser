module Util.Maybe exposing ((?), isDefined, orElse)

(?) : Maybe a -> a -> a
(?) = flip Maybe.withDefault

isDefined : Maybe a -> Bool
isDefined m =
  case m of
    Just _ ->
      True
    Nothing ->
      False

orElse : Maybe a -> Maybe a -> Maybe a
orElse m2 =
  Maybe.withDefault m2 << Maybe.map Just
