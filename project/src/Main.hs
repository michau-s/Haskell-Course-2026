module Main where

import System.Environment (getArgs)
import Text.Megaparsec (errorBundlePretty)
import Parser (parseWindowData)
import Layout (resolveWindow)
import Render (renderSvg)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filepath] -> processFile filepath
    _          -> putStrLn "Usage: cabal run UiLayoutLang -- <path-to-layout-file.txt>"

processFile :: FilePath -> IO ()
processFile filepath = do
  content <- readFile filepath
  case parseWindowData filepath content of
    Left err -> putStrLn $ "Parsing failed:\n" ++ errorBundlePretty err
    Right win -> do
      putStrLn $ "Successfully parsed file: " ++ filepath
      
      -- 1. Resolve Layout
      let resolved = resolveWindow win
      putStrLn "\n--- Resolved Layout Tree ---"
      print resolved
      
      -- 2. Render to SVG
      let svgContent = renderSvg win resolved
      let outPath = filepath ++ ".svg"
      writeFile outPath svgContent
      putStrLn $ "\n Successfully exported layout to: " ++ outPath