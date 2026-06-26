module Render (renderSvg) where

import Types
import Data.Maybe (fromMaybe)

renderNode :: Resolved -> String
renderNode (Resolved x y w h col children) =
  let 
      fillColor = fromMaybe "transparent" col
      rect = "<rect x=\"" ++ show x ++ 
             "\" y=\"" ++ show y ++ 
             "\" width=\"" ++ show w ++ 
             "\" height=\"" ++ show h ++ 
             "\" fill=\"" ++ fillColor ++ 
             "\" stroke=\"#333333\" stroke-width=\"1\" />\n"
             
      kids = concatMap renderNode children
  in rect ++ kids

-- Wraps the generated rects in the main <svg> boilerplate
renderSvg :: Window -> Resolved -> String
renderSvg (Window name w h _) resolved =
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" ++
  "\n" ++
  "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"" ++ show w ++ 
  "\" height=\"" ++ show h ++ "\" viewBox=\"0 0 " ++ show w ++ " " ++ show h ++ "\">\n" ++
  renderNode resolved ++
  "</svg>\n"