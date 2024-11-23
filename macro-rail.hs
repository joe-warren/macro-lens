#!/usr/bin/env stack
{- stack script --resolver lts-22.6 
    --package linear
    --package waterfall-cad
    --extra-dep waterfall-cad-0.4.0.0
    --extra-dep opencascade-hs-0.4.0.0
-}

import qualified Waterfall
import Linear
import Data.Function ((&))

nema17Holes :: Waterfall.Solid
nema17Holes = 
    let sep = 31
        h = 100
        rScrew = 3/2
        screwHole = Waterfall.centeredCylinder & 
            Waterfall.scale (V3 rScrew rScrew h)
        rCenter = 22.5/2
        centerHole = Waterfall.centeredCylinder &
            Waterfall.scale (V3 rCenter rCenter h)
    in 
        centerHole <> 
        mconcat
          [ screwHole & 
                Waterfall.translate (V3 x y 0)
            | x <- [-sep/2, sep/2]
            , y <- [-sep/2, sep/2]
          ]

base :: Waterfall.Solid
base = 
    let 
        railBaseL = 60
        railDialL = 19
        clearance = 5
        nemaShaftL = 24
        nemaL = 48
        baseL = 
            railBaseL + railDialL + clearance + nemaShaftL + nemaL 

        baseT = 60
        
        nemaAxisAboveBase = 42/2
        railAxisAboveRailBase = 12
        railBaseAboveBase = nemaAxisAboveBase - railAxisAboveRailBase
        baseH = 6

        basePlate = Waterfall.unitCube &
            Waterfall.scale (V3 baseL baseT baseH)

        nemaPlateT = 10
        nemaHolesPositioned = nema17Holes &
            Waterfall.rotate (unit _y) (pi/2) &
            Waterfall.translate (unit _z ^* (nemaAxisAboveBase + baseH)) &
            Waterfall.translate (unit _y ^* (baseT/2)) &
            Waterfall.translate (unit _x ^* (baseL - nemaL))
        nemaPlate = Waterfall.unitCube & 
            Waterfall.scale (V3 nemaPlateT baseT (baseH + 42)) &
            Waterfall.translate (unit _x ^* (baseL - nemaL - nemaPlateT)) &
            (`Waterfall.difference` nemaHolesPositioned)
        railRiser = Waterfall.unitCube &
            Waterfall.scale (V3 railBaseL baseT (baseH + railBaseAboveBase))

        railScrewR = 6/2
        railScrewCapR = 11/2
        railScrewOffset = 48
        railScrew = 
            ((Waterfall.centeredCylinder & Waterfall.scale (V3 railScrewR railScrewR 100)) <>
                (Waterfall.centeredCylinder & Waterfall.scale (V3 railScrewCapR railScrewCapR 6))) &
                Waterfall.translate (V3 railScrewOffset (baseT/2) 0)

        railGuides = mconcat 
            [ Waterfall.unitCylinder & Waterfall.scale (V3 0.5 0.5 railBaseL) & 
                Waterfall.rotate (unit _y) (pi/2) &
                Waterfall.translate (V3 0 (baseT/2 + yOff) (baseH + railBaseAboveBase))
                | yOff <- [-14, 14]
            ]

    in (basePlate <> nemaPlate <> railRiser <> railGuides) `Waterfall.difference` railScrew

main :: IO ()
main = do
    Waterfall.writeSTL 0.1 "macro-rail-base.stl" base