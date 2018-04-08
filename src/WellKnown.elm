module WellKnown exposing (..)

import Combine exposing (parse)
import GeoJson exposing (Geometry)
import WellKnown.Parse exposing (geometryParser)


read : String -> Result String Geometry
read input =
    case parse geometryParser input of
        Ok ( _, stream, result ) ->
            Ok result

        Err ( _, stream, errors ) ->
            Err (String.join "\n" errors)