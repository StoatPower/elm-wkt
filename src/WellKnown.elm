module WellKnown exposing (..)

import GeoJson exposing (Geometry(..), Position)
import Combine as C exposing (..)
import Combine.Num as C exposing (..)


read : String -> Result String Geometry
read input =
    case C.parse geometryParser input of
        Ok ( _, stream, result ) ->
            Ok result

        Err ( _, stream, errors ) ->
            Err (String.join "\n" errors)


geometryParser : Parser () Geometry
geometryParser =
    C.choice
        [ simpleGeometryParser
        , geometryCollectionParser
        ]


geometryCollectionParser : Parser () Geometry
geometryCollectionParser =
    (C.string "GEOMETRYCOLLECTION")
        *> C.whitespace
        *> (GeometryCollection <$> C.parens simpleGeometryListParser)


simpleGeometryListParser : Parser () (List Geometry)
simpleGeometryListParser =
    C.sepBy ((C.string ",") *> C.whitespace) (C.parens simpleGeometryParser)


simpleGeometryParser : Parser () Geometry
simpleGeometryParser =
    C.choice
        [ pointParser
        , multiPointParser
        , lineStringParser
        , multiLineStringParser
        , polygonParser
        , multiPolygonParser
        ]


multiPolygonParser : Parser () Geometry
multiPolygonParser =
    (C.string "MULTIPOLYGON") *> C.whitespace *> (MultiPolygon <$> C.parens positionListListListParser)


polygonParser : Parser () Geometry
polygonParser =
    (C.string "POLYGON") *> C.whitespace *> (Polygon <$> C.parens positionListListParser)


multiLineStringParser : Parser () Geometry
multiLineStringParser =
    (C.string "MULTILINESTRING") *> C.whitespace *> (MultiLineString <$> C.parens positionListListParser)


lineStringParser : Parser () Geometry
lineStringParser =
    (C.string "LINESTRING") *> C.whitespace *> (LineString <$> C.parens positionListParser)


multiPointParser : Parser () Geometry
multiPointParser =
    (C.string "MULTIPOINT") *> C.whitespace *> (MultiPoint <$> C.parens positionListParser)


pointParser : Parser () Geometry
pointParser =
    (C.string "POINT") *> C.whitespace *> (Point <$> C.parens positionParser)


positionListListListParser : Parser () (List (List (List Position)))
positionListListListParser =
    C.sepBy ((C.string ",") *> C.whitespace) (C.parens positionListListParser)


positionListListParser : Parser () (List (List Position))
positionListListParser =
    C.sepBy ((C.string ",") *> C.whitespace) (C.parens positionListParser)


positionListParser : Parser () (List Position)
positionListParser =
    C.sepBy ((C.string ",") *> C.whitespace) positionParser


positionParser : Parser () Position
positionParser =
    C.choice [ positionNoParensParser, positionWithParensParser ]


positionNoParensParser : Parser () Position
positionNoParensParser =
    toPosition <$> numberParser <* C.whitespace <*> numberParser <*> (C.whitespace *> optional 0.0 numberParser)


positionWithParensParser : Parser () Position
positionWithParensParser =
    C.parens positionNoParensParser


numberParser : Parser () Float
numberParser =
    C.choice [ C.float, toFloat <$> C.int ]


toPosition : Float -> Float -> Float -> Position
toPosition lon lat alt =
    ( lon, lat, alt )
