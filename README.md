# elm-wkt-exploration

### Install
Not yet published as an elm package. Stay tuned...

### Test
```
npm install
npm run test
```

## Usage

### Read WKT into GeoJson Geometries
```
import WellKnown exposing (read)

wkt = "GEOMETRYCOLLECTION (POINT (4 6), LINESTRING (4 6, 7 10))"

geometry : Result String Geometry
geometry =
    read wkt
```

### Write GeoJson Geometries to a WKT String
```
import WellKnown exposing (write)

geometry : Geometry
geometry = 
    GeometryCollection
        [ Point ( 4.0, 6.0, 0.0 )
        , LineString [ ( 4.0, 6.0, 0.0 ), ( 7.0, 10.0, 0.0 ) ]
        ]

wkt : String
wkt =
    write geometry
```
