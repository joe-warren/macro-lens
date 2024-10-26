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
import Data.Either (partitionEithers)
import Data.Monoid (Endo (..))

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
    
createHelicalArcSegment :: Double -> Double -> Double -> Double -> Waterfall.Path 
createHelicalArcSegment r1 r2 pitch incAngle =
  let alpha = incAngle / 2 -- half included angle
      p = pitch/(2* pi) --  helix height per radian
      a1x = r1 * cos alpha
      a1y = r1 * sin alpha
      a2x = r2 * cos alpha
      a2y = r2 * sin alpha
      b1 = p * alpha * (r1 - a1x) * (3*r1 - a1x)/(a1y * (4*r1 - a1x) * tan alpha)
      b2 = p * alpha * (r1 - a1x) * (3*r1 - a1x)/(a1y * (4*r1 - a1x) * tan alpha)
      b'0 = V3 a1x (negate a1y) (negate alpha*p)
      b'1 = V3 ((4*r1 - a1x)/3) (negate $ (r1 - a1x)*(3*r1 - a1x)/(3*a1y)) (negate b1)
      b'2 = V3 ((4*r2 - a2x)/3) ((r2 - a2x)*(3*r2 - a2x)/(3*a2y)) b2
      b'3 = V3 a2x a2y (alpha*p)
  in Waterfall.bezier b'0 b'1 b'2 b'3

createHelicalArc :: Double -> Double -> Double -> Double -> Integer -> Waterfall.Path
createHelicalArc r1 r2 height nTurns segments = 
    let pitch = height / nTurns
        segments' = fromIntegral segments
        heightPer = height / segments'
        rotationPer = nTurns * pi * 2 / segments' 
     in mconcat [ 
         let i' = fromIntegral i
             frac = i' / segments'
             frac' = (i'+1) / segments'
             ra = frac * r1 + (1-frac) * r2 
             rb = frac' * r1 + (1-frac') * r2
         in Waterfall.translate ((i' + 0.5) * heightPer *^ unit _z) $
                Waterfall.rotate (unit _z) ((i' + 0.5) * rotationPer) $ 
                    createHelicalArcSegment ra rb pitch rotationPer
         | i <- [0..segments-1]]


lensCone :: Waterfall.Solid
lensConeWithScrew :: Waterfall.Solid
(lensCone, lensConeWithScrew) = 
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
    rEnd = 28/2
    borderHeight = 2.5
    borderWidth = 1.5
    thickness = 4
    helixOffset = 20
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
    cone = Waterfall.rotate (unit _y) pi $ Waterfall.revolution profile
    helix = Waterfall.translate (coneStart *^ unit _z) $
        createHelicalArc rEnd rBase (coneEnd - coneStart) (2/3) (12)
    helixProfile = Waterfall.unitCircle
    helix' = Waterfall.sweep helix helixProfile
    helixStart = Waterfall.sweep (Waterfall.line (V3 rBase 0 helixOffset) (V3 rBase 0 coneStart)) helixProfile
    helixEnd = Waterfall.sweep (Waterfall.line (V3 rEnd 0 coneEnd) (V3 rEnd 0 (lensFlangeDistance+borderHeight))) helixProfile
    helixJoints = mconcat $ Waterfall.translate <$> 
        [ V3 rBase 0 helixOffset
        , V3 rBase 0 coneStart
        , V3 rEnd 0 coneEnd
        ] <*> (pure Waterfall.unitSphere)
    fullHelix = Waterfall.rotate (unit _y) pi (helix' <> helixStart <> helixEnd <> helixJoints)
    (additiveHelixes, subtractiveHelixes) = partitionEithers $ zipWith ($) (cycle [Left, Right, Right, Right])
         (take 24 $ iterate (Waterfall.rotate (unit _z) (pi/12)) $ fullHelix)
    helixify =  (`Waterfall.difference` mconcat subtractiveHelixes ). ( <> mconcat additiveHelixes)
  in (cone, helixify cone )
    
subtractCone :: Waterfall.Solid -> Waterfall.Solid
subtractCone = let offset = Waterfall.offset 0.1 1e-6 
    in (`Waterfall.difference` (offset lensCone))

main :: IO ()
main = do
    -- Model is positioned with the origin at the center of the flange plane
    -- with the direction of the lens in the negative Z direction
    initialMount <- Waterfall.readSolid "Nikon Z-Mount.STEP"
    Waterfall.writeSTL 0.1 "lens-base-a.stl" (subtractCone $ addCylinderToBase initialMount)
    Waterfall.writeSTL 0.1 "lens-cone.stl" lensConeWithScrew
