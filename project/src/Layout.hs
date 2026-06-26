module Layout (resolveWindow, resolveBox) where

import Types

-- Computes absolute coordinates top-down.
-- containerW/H is for resolving percentages.
-- maxW/H is the remaining scpace used for clipping (Overflow Rule).
resolveBox :: Int -> Int -> Int -> Int -> Int -> Int -> Layout -> Resolved
resolveBox x y containerW containerH maxW maxH (Box props children) =
  let 
      w = case width props of
            Px n  -> n
            Pct p -> round (p * fromIntegral containerW)
      h = case height props of
            Px n  -> n
            Pct p -> round (p * fromIntegral containerH)
            
      -- Apply clipping to ensure no child sticks out (Overflow rule)
      actualW = max 0 (min w maxW)
      actualH = max 0 (min h maxH)

      resChildren = doLayout (dir props) x y actualW actualH actualW actualH children
  in Resolved x y actualW actualH (color props) resChildren

-- Underflow rule: Start-aligned (leftover space remains trailing/empty)
doLayout :: Direction -> Int -> Int -> Int -> Int -> Int -> Int -> [Layout] -> [Resolved]
doLayout _ _ _ _ _ _ _ [] = []
doLayout Row cx cy parW parH remW remH (c:cs) =
  let r = resolveBox cx cy parW parH remW parH c
      usedW = rw r
  in r : doLayout Row (cx + usedW) cy parW parH (remW - usedW) remH cs
doLayout Col cx cy parW parH remW remH (c:cs) =
  let r = resolveBox cx cy parW parH parW remH c
      usedH = rh r
  in r : doLayout Col cx (cy + usedH) parW parH remW (remH - usedH) cs

resolveWindow :: Window -> Resolved
resolveWindow (Window _ w h layout) =
  resolveBox 0 0 w h w h layout