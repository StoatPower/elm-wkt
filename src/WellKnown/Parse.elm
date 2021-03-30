module WellKnown.Parse exposing (..)

import GeoJson exposing (Geometry(..), Position)
import Parser exposing (..)
import Parser.Extras exposing (..)


geometryParser : Parser Geometry
geometryParser =
    oneOf
        [ simpleGeometryParser
        , geometryCollectionParser
        ]


geometryCollectionParser : Parser Geometry
geometryCollectionParser =
    succeed GeometryCollection
        |. prefix "GEOMETRYCOLLECTION"
        |= parens simpleGeometryListParser


simpleGeometryListParser : Parser (List Geometry)
simpleGeometryListParser =
    sequence
        { start = ""
        , separator = ","
        , end = ""
        , spaces = spaces
        , item = simpleGeometryParser
        , trailing = Parser.Optional
        }


simpleGeometryParser : Parser Geometry
simpleGeometryParser =
    oneOf
        [ pointParser
        , multiPointParser
        , lineStringParser
        , multiLineStringParser
        , polygonParser
        , multiPolygonParser
        ]


multiPolygonParser : Parser Geometry
multiPolygonParser =
    succeed MultiPolygon
        |. prefix "MULTIPOLYGON"
        |= parens positionListListListParser


polygonParser : Parser Geometry
polygonParser =
    succeed Polygon
        |. prefix "POLYGON"
        |= parens positionListListParser


multiLineStringParser : Parser Geometry
multiLineStringParser =
    succeed MultiLineString
        |. prefix "MULTILINESTRING"
        |= parens positionListListParser


lineStringParser : Parser Geometry
lineStringParser =
    succeed LineString
        |. prefix "LINESTRING"
        |= parens positionListParser


multiPointParser : Parser Geometry
multiPointParser =
    succeed MultiPoint
        |. prefix "MULTIPOINT"
        |= parens positionListParser


pointParser : Parser Geometry
pointParser =
    succeed Point
        |. prefix "POINT"
        |= parens positionParser


positionListListListParser : Parser (List (List (List Position)))
positionListListListParser =
    sequence
        { start = ""
        , separator = ","
        , end = ""
        , spaces = spaces
        , item = parens positionListListParser
        , trailing = Parser.Optional
        }


positionListListParser : Parser (List (List Position))
positionListListParser =
    sequence
        { start = ""
        , separator = ","
        , end = ""
        , spaces = spaces
        , item = parens positionListParser
        , trailing = Parser.Optional
        }


positionListParser : Parser (List Position)
positionListParser =
    sequence
        { start = ""
        , separator = ","
        , end = ""
        , spaces = spaces
        , item = positionParser
        , trailing = Parser.Optional
        }


positionParser : Parser Position
positionParser =
    oneOf [ positionNoParensParser, positionWithParensParser ]


positionNoParensParser : Parser Position
positionNoParensParser =
    succeed toPosition
        |= float
        |. commaWhitespace
        |= float
        |= altitudePostfix


positionWithParensParser : Parser Position
positionWithParensParser =
    parens positionNoParensParser


toPosition : Float -> Float -> Maybe Float -> Position
toPosition lon lat maybeAltitude =
    ( lon, lat, Maybe.withDefault 0.0 maybeAltitude )


altitudePostfix : Parser (Maybe Float)
altitudePostfix =
    oneOf
        [ succeed identity |. commaWhitespace |= maybeFloat
        , succeed Nothing
        ]


maybeFloat : Parser (Maybe Float)
maybeFloat =
    oneOf
        [ map Just float
        , succeed Nothing
        ]


commaWhitespace : Parser ()
commaWhitespace =
    oneOf
        [ spaces
        , succeed ()
            |. symbol ","
            |. spaces
        ]


prefix : String -> Parser ()
prefix p =
    succeed ()
        |. keyword p
        |. spaces
