module WellKnown exposing (read, write)

{-| Parse and unparse [Well-known Text (WKT)](https://en.wikipedia.org/wiki/Well-known_text) strings to and from
Elm `Geometry` types from [elm-geojson](http://package.elm-lang.org/packages/mgold/elm-geojson/2.0.0/).

Supported Geometries include `POINT`, `MULTIPOINT`, `LINESTRING`, `MULTILINESTRING`, `POLYGON`,
`MULTIPOLYGON`, and `GEOMETRYCOLLECTION`.

#Read
@docs read

#Write
@docs write
-}

import Combine exposing (parse)
import Formatting exposing (print)
import GeoJson exposing (Geometry)
import WellKnown.Parse exposing (geometryParser)
import WellKnown.Unparse exposing (geometryFormat)


{-| Attempt to read a WKT string and parse it into a `Geometry` type from `elm-geojson`.
-}
read : String -> Result String Geometry
read input =
    case parse geometryParser input of
        Ok ( _, stream, result ) ->
            Ok result

        Err ( _, stream, errors ) ->
            Err (String.join "\n" errors)


{-| Write a `Geometry` type from `elm-geojson` to a `String`, formatted as a WKT string.
-}
write : Geometry -> String
write input =
    print geometryFormat input