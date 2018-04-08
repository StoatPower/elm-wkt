module Tests exposing (..)

import WellKnown.Parse exposing (..)
import WellKnown.Unparse exposing (..)
import GeoJson exposing (Geometry(..), Position)
import Combine as C
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


testParser : C.Parser () a -> String -> a -> Expectation
testParser parser input expectedResult =
    case C.parse parser input of
        Ok ( _, _, result ) ->
            Expect.equal result expectedResult

        Err ( _, _, errors ) ->
            Expect.fail (String.join "\n" errors)


suite : Test
suite =
    describe "WKT to GeoJSON"
        [ describe "parse number"
            [ test "positive float" <|
                \_ ->
                    testParser numberParser "1.1" 1.1
            , test "negative float" <|
                \_ ->
                    testParser numberParser "-1.1" -1.1
            , test "positive int" <|
                \_ ->
                    testParser numberParser "1" 1
            , test "negative int" <|
                \_ ->
                    testParser numberParser "-1" -1
            ]
        , describe "parse Position with no parens"
            [ test "integer position with no altitude" <|
                \_ ->
                    testParser positionParser "1 1" ( 1.0, 1.0, 0.0 )
            , test "float position with no altitude" <|
                \_ ->
                    testParser positionParser "1.1 1.1" ( 1.1, 1.1, 0.0 )
            , test "integer position with altitude" <|
                \_ ->
                    testParser positionParser "1 1 2" ( 1.0, 1.0, 2.0 )
            , test "float position with altitude" <|
                \_ ->
                    testParser positionParser "1.1 1.1 2.2" ( 1.1, 1.1, 2.2 )
            ]
        , describe "parse Position with parens"
            [ test "integer position with no altitude" <|
                \_ ->
                    testParser positionParser "(1 1)" ( 1.0, 1.0, 0.0 )
            , test "float position with no altitude" <|
                \_ ->
                    testParser positionParser "(1.1 1.1)" ( 1.1, 1.1, 0.0 )
            , test "integer position with altitude" <|
                \_ ->
                    testParser positionParser "(1 1 2)" ( 1.0, 1.0, 2.0 )
            , test "float position with altitude" <|
                \_ ->
                    testParser positionParser "(1.1 1.1 2.2)" ( 1.1, 1.1, 2.2 )
            ]
        , describe "parse a list of Positions"
            (let
                expected =
                    [ ( 1.0, 1.0, 0.0 ), ( 2.0, 2.0, 0.0 ) ]
             in
                [ test "list of float positions without parens" <|
                    \_ ->
                        testParser positionListParser "1.0 1.0, 2.0 2.0" expected
                , test "list of float positions with parens" <|
                    \_ ->
                        testParser positionListParser "(1.0 1.0), (2.0 2.0)" expected
                , test "list of int positions without parens" <|
                    \_ ->
                        testParser positionListParser "1 1, 2 2" expected
                , test "list of int positions with parens" <|
                    \_ ->
                        testParser positionListParser "(1 1), (2 2)" expected
                ]
            )
        , describe "parse a list of a list of Positions"
            (let
                expected =
                    [ [ ( 1.0, 1.0, 0.0 ), ( 2.0, 2.0, 0.0 ) ], [ ( 3.0, 3.0, 0.0 ), ( 4.0, 4.0, 0.0 ) ] ]
             in
                [ test "list of a list of int positions without position parens and no altitude" <|
                    \_ ->
                        testParser positionListListParser "(1 1, 2 2), (3 3, 4 4)" expected
                , test "list of a list of int positions with position parens and altitude" <|
                    \_ ->
                        testParser positionListListParser "((1 1 0), (2 2 0)), ((3 3 0), (4 4 0))" expected
                ]
            )
        , describe "parse a list of a list of a list of Positions"
            [ test "list of a list of a list of int positions without position parens and no altitude" <|
                \_ ->
                    testParser
                        positionListListListParser
                        "((1 1, 2 2), (3 3, 4 4)), ((1 1, 2 2), (3 3, 4 4))"
                        [ [ [ ( 1.0, 1.0, 0.0 ), ( 2.0, 2.0, 0.0 ) ], [ ( 3.0, 3.0, 0.0 ), ( 4.0, 4.0, 0.0 ) ] ]
                        , [ [ ( 1.0, 1.0, 0.0 ), ( 2.0, 2.0, 0.0 ) ], [ ( 3.0, 3.0, 0.0 ), ( 4.0, 4.0, 0.0 ) ] ]
                        ]
            ]
        , describe "parse Point"
            [ test "point geometry (float) with no altitude" <|
                \_ ->
                    testParser pointParser "POINT (1.0 1.0)" (Point ( 1.0, 1.0, 0.0 ))
            , test "point geometry (float) with altitude" <|
                \_ ->
                    testParser pointParser "POINT (1.1 1.1 2.2)" (Point ( 1.1, 1.1, 2.2 ))
            ]
        , describe "parse MultiPoint"
            [ test "multipoint geometry (float) with no altitude" <|
                \_ ->
                    testParser multiPointParser "MULTIPOINT (1.0 1.0, 2.0 2.0)" (MultiPoint [ ( 1.0, 1.0, 0.0 ), ( 2.0, 2.0, 0.0 ) ])
            , test "multipoint geometry (int) with parens and altitude" <|
                \_ ->
                    testParser multiPointParser "MULTIPOINT ((1 1 2), (2 2 2))" (MultiPoint [ ( 1.0, 1.0, 2.0 ), ( 2.0, 2.0, 2.0 ) ])
            ]
        , describe "parse LineString"
            [ test "linestring geometry (int) without position parens and no altitude" <|
                \_ ->
                    testParser lineStringParser "LINESTRING (1 1, 2 2)" (LineString [ ( 1.0, 1.0, 0.0 ), ( 2.0, 2.0, 0.0 ) ])
            ]
        , describe "parse MultiLineString"
            [ test "multilinestring geometry (float) with position parens and altitude" <|
                \_ ->
                    testParser
                        multiLineStringParser
                        "MULTILINESTRING (((1.0 1.0 1.0), (2.0 2.0 2.0)), ((3.0 3.0 3.0), (4.0 4.0 4.0)))"
                        (MultiLineString [ [ ( 1.0, 1.0, 1.0 ), ( 2.0, 2.0, 2.0 ) ], [ ( 3.0, 3.0, 3.0 ), ( 4.0, 4.0, 4.0 ) ] ])
            ]
        , describe "parse Polygon"
            [ test "polygon geometry (int) without position parens and no altitude" <|
                \_ ->
                    testParser
                        polygonParser
                        "POLYGON ((30 10, 40 40, 20 40, 10 20, 30 10))"
                        (Polygon [ [ ( 30.0, 10.0, 0.0 ), ( 40.0, 40.0, 0.0 ), ( 20.0, 40.0, 0.0 ), ( 10.0, 20.0, 0.0 ), ( 30.0, 10.0, 0.0 ) ] ])
            , test "polygon geometry with hole (int) without position parens and no altitude" <|
                \_ ->
                    testParser
                        polygonParser
                        "POLYGON ((35 10, 45 45, 15 40, 10 20, 35 10),(20 30, 35 35, 30 20, 20 30))"
                        (Polygon
                            [ [ ( 35.0, 10.0, 0.0 ), ( 45.0, 45.0, 0.0 ), ( 15.0, 40.0, 0.0 ), ( 10.0, 20.0, 0.0 ), ( 35.0, 10.0, 0.0 ) ]
                            , [ ( 20.0, 30.0, 0.0 ), ( 35.0, 35.0, 0.0 ), ( 30.0, 20.0, 0.0 ), ( 20.0, 30.0, 0.0 ) ]
                            ]
                        )
            ]
        , describe "parse MultiPolygon"
            [ test "multipolygon geometry (int) without position parens and no altitude" <|
                \_ ->
                    testParser
                        multiPolygonParser
                        "MULTIPOLYGON (((30 20, 45 40, 10 40, 30 20)), ((15 5, 40 10, 10 20, 5 10, 15 5)))"
                        (MultiPolygon
                            [ [ [ ( 30.0, 20.0, 0.0 ), ( 45.0, 40.0, 0.0 ), ( 10.0, 40.0, 0.0 ), ( 30.0, 20.0, 0.0 ) ] ]
                            , [ [ ( 15.0, 5.0, 0.0 ), ( 40.0, 10.0, 0.0 ), ( 10.0, 20.0, 0.0 ), ( 5.0, 10.0, 0.0 ), ( 15.0, 5.0, 0.0 ) ] ]
                            ]
                        )
            ]
        , describe "parse GeometryCollection"
            [ test "geometrycollection geometry (int) without position parens and no altitude" <|
                \_ ->
                    testParser
                        geometryCollectionParser
                        "GEOMETRYCOLLECTION (POINT (4 6), LINESTRING (4 6, 7 10))"
                        (GeometryCollection
                            [ Point ( 4.0, 6.0, 0.0 )
                            , LineString [ ( 4.0, 6.0, 0.0 ), ( 7.0, 10.0, 0.0 ) ]
                            ]
                        )
            ]
        ]
