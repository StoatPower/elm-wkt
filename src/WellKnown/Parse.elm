module WellKnown.Parse exposing (..)

import GeoJson exposing (Geometry(..), Position)
import Combine as C exposing (..)
import Combine.Num as C exposing (..)


geometryParser : Parser () Geometry
geometryParser =
    choice
        [ simpleGeometryParser
        , geometryCollectionParser
        ]


geometryCollectionParser : Parser () Geometry
geometryCollectionParser =
    prefix "GEOMETRYCOLLECTION" *> (GeometryCollection <$> parens simpleGeometryListParser)


simpleGeometryListParser : Parser () (List Geometry)
simpleGeometryListParser =
    sepBy commaWhitespace (parens simpleGeometryParser)


simpleGeometryParser : Parser () Geometry
simpleGeometryParser =
    choice
        [ pointParser
        , multiPointParser
        , lineStringParser
        , multiLineStringParser
        , polygonParser
        , multiPolygonParser
        ]


multiPolygonParser : Parser () Geometry
multiPolygonParser =
    prefix "MULTIPOLYGON" *> (MultiPolygon <$> parens positionListListListParser)


polygonParser : Parser () Geometry
polygonParser =
    prefix "POLYGON" *> (Polygon <$> parens positionListListParser)


multiLineStringParser : Parser () Geometry
multiLineStringParser =
    prefix "MULTILINESTRING" *> (MultiLineString <$> parens positionListListParser)


lineStringParser : Parser () Geometry
lineStringParser =
    prefix "LINESTRING" *> (LineString <$> parens positionListParser)


multiPointParser : Parser () Geometry
multiPointParser =
    prefix "MULTIPOINT" *> (MultiPoint <$> parens positionListParser)


pointParser : Parser () Geometry
pointParser =
    prefix "POINT" *> (Point <$> parens positionParser)


positionListListListParser : Parser () (List (List (List Position)))
positionListListListParser =
    sepBy commaWhitespace (parens positionListListParser)


positionListListParser : Parser () (List (List Position))
positionListListParser =
    sepBy commaWhitespace (parens positionListParser)


positionListParser : Parser () (List Position)
positionListParser =
    sepBy commaWhitespace positionParser


positionParser : Parser () Position
positionParser =
    choice [ positionNoParensParser, positionWithParensParser ]


positionNoParensParser : Parser () Position
positionNoParensParser =
    toPosition <$> numberParser <* whitespace <*> numberParser <*> (whitespace *> optional 0.0 numberParser)


positionWithParensParser : Parser () Position
positionWithParensParser =
    parens positionNoParensParser


numberParser : Parser () Float
numberParser =
    choice [ float, toFloat <$> int ]


toPosition : Float -> Float -> Float -> Position
toPosition lon lat alt =
    ( lon, lat, alt )


commaWhitespace : Parser s String
commaWhitespace =
    (string ",") *> whitespace


prefix : String -> Parser s String
prefix p =
    (string p) *> whitespace