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

addCylinderToBase :: Waterfall.Solid -> Waterfall.Solid
addCylinderToBase s = 
    let 
        outerR = 70/2
        outerC = Waterfall.unitCylinder
            & Waterfall.rotate (unit _y) pi
            & Waterfall.scale (V3 outerR outerR 10) 
            & Waterfall.translate (negate 2 *^ unit _z) 
        innerR = 48/2
        innerC = Waterfall.centeredCylinder 
            & Waterfall.scale (V3 innerR innerR 100)
    in s <> (outerC `Waterfall.difference` innerC)
    

lensCone :: Waterfall.Solid
lensCone = 
  let
    baseOffset = 5
    coneStart = 30
    flangeFocalDistance = 16
    tubeLength = 160
    rBase = 60/2
    rLens = 20.32/2
    dLens = 4
    coneEnd = tubeLength - flangeFocalDistance - dLens
    lensFlangeDistance = tubeLength - flangeFocalDistance
    rEnd = 30/2
    borderHeight = 2.5
    borderWidth = 1.5
    thickness = 2
    v y z = V2 y z
    profile = Waterfall.closeLoop $ Waterfall.pathFrom (v rBase baseOffset)
        [ Waterfall.lineTo (v rBase coneStart)
        , Waterfall.lineTo (v rEnd coneEnd)
        , Waterfall.lineRelative (v 0 (dLens + borderHeight))
        , Waterfall.lineRelative (v (negate borderWidth) 0)
        , Waterfall.lineRelative (v 0 (negate borderHeight))
        , Waterfall.lineTo (v rLens lensFlangeDistance)
        , Waterfall.lineRelative (v 0 (negate dLens))
        , Waterfall.lineTo (v (rBase-thickness) coneStart)
        , Waterfall.lineTo (v (rBase-thickness) baseOffset)
        ]
  in Waterfall.rotate (unit _y) pi $ Waterfall.revolution profile
    
subtractCone :: Waterfall.Solid -> Waterfall.Solid
subtractCone = let offset = Waterfall.offset 0.1 1e-6 
    in (`Waterfall.difference` (offset lensCone))

main :: IO ()
main = do
    -- Model is positioned with the origin at the center of the flange plane
    -- with the direction of the lens in the negative Z direction
    initialMount <- Waterfall.readSolid "Nikon Z-Mount.STEP"
    Waterfall.writeSTL 0.1 "lens-base-a.stl" (subtractCone $ addCylinderToBase initialMount)
    Waterfall.writeSTL 0.1 "lens-cone.stl" lensCone
