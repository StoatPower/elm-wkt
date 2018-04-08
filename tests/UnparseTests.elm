module UnparseTests exposing (..)

import WellKnown.Unparse exposing (..)
import Formatting as Fmt exposing (..)
import GeoJson exposing (Geometry(..), Position)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


testUnparser : Format String (a -> String) -> a -> String -> Expectation
testUnparser formatter input expectedResult =
    Expect.equal (Fmt.print formatter input) expectedResult


suite : Test
suite =
    describe "GeoJSON to WKT"
        [ describe "miscellaneous"
            [ test "parenthesize" <|
                \_ ->
                    testUnparser parenthesize "wrap me up" "(wrap me up)"
            ]
        , describe "unparse Position"
            [ test "integer position with no altitude" <|
                \_ ->
                    testUnparser positionFormat ( 1.0, 1.0, 0.0 ) "1 1"
            , test "float position with altitude" <|
                \_ ->
                    testUnparser positionFormat ( 1.5, 2.5, 3.2) "1.5 2.5 3.2"
            ]
        , describe "unparse list of Positions"
            [ test "list of int positions with no altitude" <|
                \_ ->
                    testUnparser 
                        positionListFormat 
                        [ ( 1.0, 1.0, 0.0 ), ( 2.0, 2.0, 0.0 ) ]
                        "(1 1,2 2)"
            , test "list of float positions with altitude" <|
                \_ ->
                    testUnparser
                        positionListFormat
                        [ ( 1.1, 1.1, 1.1 ), ( 2.2, 2.2, 2.2 ) ]
                        "(1.1 1.1 1.1,2.2 2.2 2.2)"
            ]
        , describe "unparse list of list of Positions"
            [ test "list of list of int positions with no altitude" <|
                \_ ->
                    testUnparser
                        positionListListFormat
                        [ [ ( 1.0, 1.0, 0.0 ), ( 2.0, 2.0, 0.0 ) ], [ ( 3.0, 3.0, 0.0 ), ( 4.0, 4.0, 0.0 ) ] ]
                        "((1 1,2 2),(3 3,4 4))"
            , test "list of list of float positions with altitude" <|
                \_ ->
                    testUnparser
                        positionListListFormat
                        [ [ ( 1.1, 1.1, 1.1 ), ( 2.2, 2.2, 2.2 ) ], [ ( 3.3, 3.3, 3.3 ), ( 4.4, 4.4, 4.4 ) ] ]
                        "((1.1 1.1 1.1,2.2 2.2 2.2),(3.3 3.3 3.3,4.4 4.4 4.4))"
            ]
        , describe "unparse list of list of list of Positions"
            [ test "list of list of list of int positions with no altitude" <|
                \_ ->
                    testUnparser
                        positionListListListFormat
                        [ [ [ ( 1.0, 1.0, 0.0 ), ( 2.0, 2.0, 0.0 ) ], [ ( 3.0, 3.0, 0.0 ), ( 4.0, 4.0, 0.0 ) ] ]
                        , [ [ ( 1.0, 1.0, 0.0 ), ( 2.0, 2.0, 0.0 ) ], [ ( 3.0, 3.0, 0.0 ), ( 4.0, 4.0, 0.0 ) ] ]
                        ]
                        "(((1 1,2 2),(3 3,4 4)),((1 1,2 2),(3 3,4 4)))"
            ]
        , describe "unparse Geometries"
            [ test "unparse Point" <|
                \_ ->
                    testUnparser
                        geometryFormat
                        (Point ( 1.0, 1.0, 0.0 ))
                        "POINT(1 1)"
            , test "unparse MultiPoint" <|
                \_ ->
                    testUnparser
                        geometryFormat
                        (MultiPoint [ ( 1.0, 1.0, 0.0 ), ( 2.0, 2.0, 0.0 ) ])
                        "MULTIPOINT(1 1,2 2)"
            , test "unparse LineString" <|
                \_ ->
                    testUnparser
                        geometryFormat
                        (LineString [ ( 1.0, 1.0, 0.0 ), ( 2.0, 2.0, 0.0 ) ])
                        "LINESTRING(1 1,2 2)"
            , test "unparse MultiLineString" <|
                \_ ->
                    testUnparser
                        geometryFormat
                        (MultiLineString [ [ ( 1.0, 1.0, 1.0 ), ( 2.0, 2.0, 2.0 ) ], [ ( 3.0, 3.0, 3.0 ), ( 4.0, 4.0, 4.0 ) ] ])
                        "MULTILINESTRING((1 1 1,2 2 2),(3 3 3,4 4 4))"
            , test "unparse Polygon" <|
                \_ ->
                    testUnparser
                        geometryFormat
                        (Polygon
                            [ [ ( 35.0, 10.0, 0.0 ), ( 45.0, 45.0, 0.0 ), ( 15.0, 40.0, 0.0 ), ( 10.0, 20.0, 0.0 ), ( 35.0, 10.0, 0.0 ) ]
                            , [ ( 20.0, 30.0, 0.0 ), ( 35.0, 35.0, 0.0 ), ( 30.0, 20.0, 0.0 ), ( 20.0, 30.0, 0.0 ) ]
                            ]
                        )
                        "POLYGON((35 10,45 45,15 40,10 20,35 10),(20 30,35 35,30 20,20 30))"
            , test "unparse MultiPolygon" <|
                \_ ->
                    testUnparser
                        geometryFormat
                        (MultiPolygon
                            [ [ [ ( 30.0, 20.0, 0.0 ), ( 45.0, 40.0, 0.0 ), ( 10.0, 40.0, 0.0 ), ( 30.0, 20.0, 0.0 ) ] ]
                            , [ [ ( 15.0, 5.0, 0.0 ), ( 40.0, 10.0, 0.0 ), ( 10.0, 20.0, 0.0 ), ( 5.0, 10.0, 0.0 ), ( 15.0, 5.0, 0.0 ) ] ]
                            ]
                        )
                        "MULTIPOLYGON(((30 20,45 40,10 40,30 20)),((15 5,40 10,10 20,5 10,15 5)))"
            ]
            , test "unparse GeometryCollection" <|
                \_ ->
                    testUnparser
                        geometryFormat
                        (GeometryCollection
                            [ Point ( 4.0, 6.0, 0.0 )
                            , LineString [ ( 4.0, 6.0, 0.0 ), ( 7.0, 10.0, 0.0 ) ]
                            ]
                        )
                        "GEOMETRYCOLLECTION(POINT(4 6),LINESTRING(4 6,7 10))"
        ]



