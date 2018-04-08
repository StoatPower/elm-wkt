module WellKnown exposing (..)

import Combine exposing (parse)
import Formatting exposing (print)
import GeoJson exposing (Geometry)
import WellKnown.Parse exposing (geometryParser)
import WellKnown.Unparse exposing (geometryFormat)


read : String -> Result String Geometry
read input =
    case parse geometryParser input of
        Ok ( _, stream, result ) ->
            Ok result

        Err ( _, stream, errors ) ->
            Err (String.join "\n" errors)


write : Geometry -> String
write input =
    print geometryFormat input