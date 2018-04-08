module WellKnown.Unparse exposing (..)

import GeoJson exposing (Geometry(..), Position)
import Formatting as Fmt exposing (..)


-- unparsePositionList : List Position -> String
-- unparsePositionList positions =
--         positions
--             |> List.map toString
--             |> List.intersperse ","
--             |> List.foldr (++) ""
--             |> parenthesize            


fmtPosition : Format r (Position -> r)
fmtPosition  =
    Format
        (\callback ((lon, lat, alt) as pos) ->
            callback <|
                case alt == 0.0 of
                    True ->
                        (toString lon) ++ " " ++ (toString lat)

                    False ->
                        (toString lon) ++ " " ++ (toString lat) ++ " " ++ (toString alt)
        )
    


-- parenthesize : String -> String
-- parenthesize s =
--     "(" ++ s ++ ")"