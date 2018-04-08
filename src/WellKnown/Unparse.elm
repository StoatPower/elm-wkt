module WellKnown.Unparse exposing (..)

import GeoJson exposing (Geometry(..), Position)
import Formatting as Fmt exposing (..)


geometryFormat : Format r (Geometry -> r) 
geometryFormat =
    Format
        (\callback geometry ->
            case geometry of
                Point position ->
                    position
                        |> print positionFormat
                        |> print parenthesize
                        |> print prefixFormat "POINT"
                        |> callback

                MultiPoint positions ->
                    positions
                        |> print positionListFormat
                        |> print prefixFormat "MULTIPOINT"
                        |> callback

                LineString positions ->
                    positions
                        |> print positionListFormat
                        |> print prefixFormat "LINESTRING"
                        |> callback

                MultiLineString positionsList ->
                    positionsList
                        |> print positionListListFormat
                        |> print prefixFormat "MULTILINESTRING"
                        |> callback

                Polygon positionsList ->
                    positionsList
                        |> print positionListListFormat
                        |> print prefixFormat "POLYGON"
                        |> callback

                MultiPolygon positionsListList ->
                    positionsListList
                        |> print positionListListListFormat
                        |> print prefixFormat "MULTIPOLYGON"
                        |> callback

                GeometryCollection geometryList ->
                    geometryList
                        |> print listFormat geometryFormat
                        |> print prefixFormat "GEOMETRYCOLLECTION"
                        |> callback
        )


positionListListListFormat : Format r (List (List (List Position)) -> r)
positionListListListFormat =
    Format
        (\callback positionsListList ->
            callback <| print listFormat positionListListFormat positionsListList
        )


positionListListFormat : Format r (List (List Position) -> r)
positionListListFormat =
    Format
        (\callback positionsList ->
            callback <| print listFormat positionListFormat positionsList
        )



positionListFormat : Format r (List Position -> r)
positionListFormat =
    Format
        (\callback positions ->
            callback <| print listFormat positionFormat positions 
        )


listFormat : Format r (Format String (a -> String) -> List a -> r)
listFormat =
    Format
        (\callback formatter items ->
            items
                |> List.map (print formatter)
                |> String.join ","
                |> print parenthesize
                |> callback
        )


positionFormat : Format r (Position -> r)
positionFormat =
    Format
        (\callback (lon, lat, alt) ->
            callback <|
                case alt == 0.0 of
                    True ->
                        (toString lon) ++ " " ++ (toString lat)

                    False ->
                        (toString lon) ++ " " ++ (toString lat) ++ " " ++ (toString alt)
        )


parenthesize : Format r (String -> r)
parenthesize =
    s "(" <> string <> s")"


prefixFormat : Format r (String -> String -> r)
prefixFormat =
    string <> string

