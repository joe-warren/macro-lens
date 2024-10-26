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

lensCap :: Waterfall.Solid
lensCap = let 
    rLens = 22/2
    clipCircleOuter 
        = Waterfall.unitCylinder & 
            Waterfall.scale (V3 (rLens + 1) (rLens+1) 3)
    clipCircleHole 
        = Waterfall.centeredCylinder & 
            Waterfall.scale (V3 (rLens) (rLens) 10)
    clipBars = 
        Waterfall.centeredCube &
        Waterfall.translate (0.5 *^ unit _z) &
        Waterfall.scale (V3 (rLens * 2 + 7) 3 3)
    clipBarsGap = 
        Waterfall.centeredCube &
        Waterfall.scale (V3 (rLens * 2 + 20) 1 10)
    clip = (clipCircleOuter <> clipBars) `Waterfall.difference` (clipCircleHole <> clipBarsGap)
    capOuterR = rLens + 4
    capInnerH = 26
    capOuter = Waterfall.unitCylinder & 
        Waterfall.scale (V3 capOuterR capOuterR (capInnerH+2))
    capInner = Waterfall.unitCylinder & 
        Waterfall.scale (V3 (rLens+2) (rLens+2) capInnerH)
    capClipGap = 
        Waterfall.centeredCube &
        Waterfall.translate (0.5 *^ unit _z) &
        Waterfall.scale (V3 (capOuterR * 2 - 2) 5 3.5)
    capHole = capInner <> capClipGap
    in (capOuter `Waterfall.difference` capHole) <> clip


main :: IO ()
main = do
    Waterfall.writeSTL 0.1 "lens-cap.stl" lensCap