module Util.Maybe exposing ((?))

(?) : Maybe a -> a -> a
(?) = flip Maybe.withDefault
