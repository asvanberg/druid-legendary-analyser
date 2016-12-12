module Util.List exposing (find)

find : (a -> Bool) -> List a -> Maybe a
find = ((<<) List.head) << List.filter
