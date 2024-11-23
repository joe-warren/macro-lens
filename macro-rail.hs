#!/usr/bin/env stack
{- stack script --resolver lts-22.6 
    --package linear
    --package lens
    --package waterfall-cad
    --extra-dep waterfall-cad-0.4.0.0
    --extra-dep opencascade-hs-0.4.0.0
-}

import qualified Waterfall
import Linear
import Control.Lens ((^.))
import Data.Function ((&))

nema17Holes :: Waterfall.Solid
nema17Holes = 
    let sep = 31
        h = 60
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

nearly :: Epsilon a => a -> a -> Bool
nearly a b = nearZero (a - b)

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
        
        baseH = 4

        nemaPlateT = 10
        nemaHolesPositioned = nema17Holes &
            Waterfall.rotate (unit _y) (pi/2) &
            Waterfall.translate (unit _z ^* (nemaAxisAboveBase + baseH)) &
            Waterfall.translate (unit _y ^* (baseT/2)) &
            Waterfall.translate (unit _x ^* (baseL - nemaL))

        railScrewR = 6/2
        railScrewCapR = 11/2
        railScrewOffset = 48
        railScrew = 
            ((Waterfall.centeredCylinder & Waterfall.scale (V3 railScrewR railScrewR 100)) <>
                (Waterfall.centeredCylinder & Waterfall.scale (V3 railScrewCapR railScrewCapR 10))) &
                Waterfall.translate (V3 railScrewOffset (baseT/2) 0)

        railGuides = mconcat 
            [ Waterfall.unitCylinder & Waterfall.scale (V3 1 1 railBaseL) & 
                Waterfall.rotate (unit _y) (pi/2) &
                Waterfall.translate (V3 0 (baseT/2 + yOff) (baseH + railBaseAboveBase))
                | yOff <- [-14, 14]
            ]

        profile = Waterfall.pathFrom zero
            [ Waterfall.lineTo (V2 0 (baseH + railBaseAboveBase))
            , Waterfall.lineRelative (V2 railBaseL 0)
            , Waterfall.lineRelative (V2 0 (-railBaseAboveBase))
            , Waterfall.lineRelative (V2 (railDialL + clearance + nemaShaftL - nemaPlateT) 0) 
            , Waterfall.lineRelative (V2 0 42)
            , Waterfall.lineRelative (V2 nemaPlateT 0)
            , Waterfall.lineRelative (V2 0 (-42))
            , Waterfall.lineRelative (V2 nemaL 0)
            , Waterfall.lineRelative (V2 0 (-baseH))
            , Waterfall.lineTo zero
            ]
        baseProfiled = Waterfall.prism baseT (Waterfall.fromPath profile) &
            Waterfall.translate (-baseT *^ unit _z) &
            Waterfall.rotate (unit _x) (pi/2)

        profileEdges = Waterfall.pathFrom zero
            [ Waterfall.lineTo (V2 0 (baseH + railBaseAboveBase))
            , Waterfall.lineRelative (V2 railBaseL 0)
            , Waterfall.lineRelative (V2 (railDialL + clearance + nemaShaftL - nemaPlateT) (42 - railBaseAboveBase))
            , Waterfall.lineRelative (V2 nemaPlateT 0)
            , Waterfall.lineRelative (V2 nemaL (-42))
            , Waterfall.lineRelative (V2 0 (-baseH))
            , Waterfall.lineTo zero
            ]

        sideT = (baseT - 42) /2
    
        baseSideA = Waterfall.prism sideT (Waterfall.fromPath profileEdges) &
            Waterfall.translate (-baseT *^ unit _z) &
            Waterfall.rotate (unit _x) (pi/2)

        baseSideB = baseSideA &
            Waterfall.translate ((sideT - baseT) *^ unit _y )
        shouldBevel  a b = ((a ^. _xy) `nearly` (b ^. _xy)) && (((a ^. _x) `nearly` 0)  || ((a ^._x) `nearly` baseL) || ((a ^. _y) `nearly` 0) || ((a ^. _y) `nearly` baseT))
        bevelF (a, b) = if shouldBevel a b then Just 8 else Nothing
        bevel = Waterfall.roundConditionalFillet bevelF


    in bevel ((baseProfiled <> railGuides <> baseSideA <> baseSideB) `Waterfall.difference` (railScrew <> nemaHolesPositioned))

coupler :: Waterfall.Solid
coupler = 
    let railDialR = 23.5/2 
        couplerR = railDialR + 2.5
        clearance = 5 
        eachHalfDepth = 10
        totalH = eachHalfDepth * 2 + clearance
        outerCyl = Waterfall.unitCylinder & 
            Waterfall.scale (V3 couplerR couplerR totalH) 
        holeDialCyl = Waterfall.centeredCylinder &
            Waterfall.scale (V3 railDialR railDialR (eachHalfDepth *2))
        holeDialGroves = mconcat 
            [ Waterfall.centeredCylinder & 
                Waterfall.scale (V3 1 1 (eachHalfDepth * 2)) &
                Waterfall.translate (unit _x ^* railDialR) &
                Waterfall.rotate (unit _z) (pi/4 + i * pi/2)
                | i <- [0..4]
            ]
        holeDial = holeDialCyl <> holeDialGroves
        rShaft = 5 /2
        shaftCut = Waterfall.centeredCube & 
            Waterfall.translate (0.5 *^ unit _x) &
            Waterfall.uScale 100 &
            Waterfall.translate (2 *^ unit _x)

        shaft = Waterfall.unitCylinder &
            Waterfall.scale (V3 rShaft rShaft (totalH *2)) &
            (`Waterfall.difference` shaftCut)

        grubScrewR = 3/2
        grubScrewHole h = Waterfall.unitCylinder &
            Waterfall.scale (V3 grubScrewR grubScrewR (couplerR *2)) &
            Waterfall.rotate (unit _y) (pi/2) &
            Waterfall.translate (unit _z ^* h)
      in outerCyl `Waterfall.difference` (holeDial <> shaft <> grubScrewHole 5 <> grubScrewHole (totalH - 5))

main :: IO ()
main = do
    Waterfall.writeSTL 0.1 "macro-rail-base.stl" base
    Waterfall.writeSTL 0.1 "macro-rail-coupler.stl" coupler