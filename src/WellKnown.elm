module WellKnown exposing (..)

import GeoJson exposing (Geometry(..), Position)
import Combine as C exposing (..)
import Combine.Char as C exposing (..)
import Combine.Num as C exposing (..)


-- read : String -> Result String Geometry
-- read input =
--     case C.parse wktParser input of
--         Ok (_, stream, result) ->
--             Ok result

--         Err (_, stream, errors) ->
--             Err (String.join "\n" errors)


-- wktParser : Parser () Geometry
-- wktParser =


-- toPosition : Float -> Float -> Maybe Float -> Position
-- toPosition lon lat alt =
--     (lon, lat, Maybe.withDefault 0.0 alt)

toPosition : Float -> Float -> Position
toPosition lon lat =
    (lon, lat, 0.0)

positionParser : Parser () Position
positionParser =
    toPosition <$> C.float <* C.whitespace <*> C.float