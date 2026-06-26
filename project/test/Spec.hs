{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec
import Test.QuickCheck
import Types
import Layout
import Parser
import Data.Either (isRight)

  -- Generate random size
instance Arbitrary Size where
  arbitrary = frequency [(3, Pct <$> choose (0.0, 1.5)), (1, Px <$> choose (10, 500))]

instance Arbitrary Direction where
  arbitrary = elements [Row, Col]

  -- Generate arbitrary width, height, direction, and leave color as Nothing
instance Arbitrary Props where
  arbitrary = Props <$> arbitrary <*> arbitrary <*> arbitrary <*> pure Nothing

  -- Generate recursive trees of layouts, naturally bounded in depth
instance Arbitrary Layout where
  arbitrary = sized genLayout
    where
      genLayout 0 = Box <$> arbitrary <*> pure []
      genLayout n = do
        props <- arbitrary
        numChildren <- choose (0, 3) :: Gen Int
        children <- mapM (\_ -> genLayout (n `div` 2)) [1..numChildren]
        return $ Box props children

main :: IO ()
main = hspec $ do
  
  describe "1. Parser Unit Tests" $ do
    it "Parses a basic window with absolute dimensions successfully" $ do
      let input = "window \"Test\" 800 x 600 { box { width: 100px, height: 100px } }"
      parseWindowData "test" input `shouldSatisfy` isRight

  describe "2. Layout Unit Tests (Per-Property)" $ do
    it "A single box with width: 100% fills the parent completely" $ do
      let layout = Box (Props (Pct 1.0) (Pct 1.0) Row Nothing) []
          win = Window "T" 500 500 layout
          res = resolveWindow win
      rw res `shouldBe` 500
      rh res `shouldBe` 500

    it "A row with two 50% children splits exactly at the middle" $ do
      let child = Box (Props (Pct 0.5) (Pct 1.0) Row Nothing) []
          layout = Box (Props (Pct 1.0) (Pct 1.0) Row Nothing) [child, child]
          win = Window "T" 1000 500 layout
          res = resolveWindow win
          kids = rChildren res
          
      length kids `shouldBe` 2
      -- First child
      rw (kids !! 0) `shouldBe` 500
      rx (kids !! 0) `shouldBe` 0
      -- Second child
      rw (kids !! 1) `shouldBe` 500
      rx (kids !! 1) `shouldBe` 500

  describe "3. End-to-End Tests (Non-Trivial Layout)" $ do
    it "Correctly computes coordinates and strictly applies the overflow rule" $ do
      -- Layout configuration:
      -- window 1000 x 1000 
      -- col { (100% x 100%)
      --   box 1: height: 200px, width 50%
      --   row 2: height: 80%, width 100%
      --     box 2a: width 300px
      --     box 2b: width 100%
      -- }
      let b1 = Box (Props (Pct 0.5) (Px 200) Row Nothing) []
          b2a = Box (Props (Px 300) (Pct 1.0) Row Nothing) []
          b2b = Box (Props (Pct 1.0) (Pct 1.0) Row Nothing) []
          row2 = Box (Props (Pct 1.0) (Pct 0.8) Row Nothing) [b2a, b2b]
          root = Box (Props (Pct 1.0) (Pct 1.0) Col Nothing) [b1, row2]
          
          win = Window "E2E" 1000 1000 root
          res = resolveWindow win
          
          [rb1, rrow2] = rChildren res
          [rb2a, rb2b] = rChildren rrow2

      -- Assert Box 1
      rw rb1 `shouldBe` 500
      rh rb1 `shouldBe` 200
      rx rb1 `shouldBe` 0
      ry rb1 `shouldBe` 0

      -- Assert Row 2
      rw rrow2 `shouldBe` 1000
      rh rrow2 `shouldBe` 800
      rx rrow2 `shouldBe` 0
      ry rrow2 `shouldBe` 200

      -- Assert Box 2a
      rw rb2a `shouldBe` 300
      rh rb2a `shouldBe` 800
      rx rb2a `shouldBe` 0
      ry rb2a `shouldBe` 200

      -- Assert Box 2b
      rw rb2b `shouldBe` 700
      rh rb2b `shouldBe` 800
      rx rb2b `shouldBe` 300
      ry rb2b `shouldBe` 200

  describe "4. Property-Based Tests (Invariants)" $ do
    it "Invariant 1: Every child's bounding box lies strictly inside its parent's" $ property $
      prop_boundsInsideParent

    it "Invariant 2: The sum of children's sizes along the layout axis is at most the parent's" $ property $
      prop_sumOfChildrenBounds


-- Recursively checks that `rx` and `ry` start inside the parent, 
-- and that the outer limits do not exceed the parent's width/height.
prop_boundsInsideParent :: Layout -> Property
prop_boundsInsideParent layout =
  let win = Window "Prop" 1000 1000 layout
      res = resolveWindow win
  in checkBounds res
  where
    checkBounds (Resolved px py pw ph _ kids) =
      let selfValid = all (\k -> rx k >= px && 
                                 ry k >= py && 
                                 (rx k + rw k) <= (px + pw) && 
                                 (ry k + rh k) <= (py + ph)) kids
      in selfValid .&&. conjoin (map checkBounds kids)

-- Traverses the original AST and Resolved Tree simultaneously to verify 
-- the axis sums don't exceed the resolved parent dimension.
prop_sumOfChildrenBounds :: Layout -> Property
prop_sumOfChildrenBounds layout =
  let win = Window "Prop" 1000 1000 layout
      res = resolveWindow win
  in checkSum layout res
  where
    checkSum (Box props kids) (Resolved _ _ pw ph _ resKids) =
      let axisSum = case dir props of
                      Row -> sum (map rw resKids)
                      Col -> sum (map rh resKids)
          limit = case dir props of
                    Row -> pw
                    Col -> ph
          validSum = axisSum <= limit
      in validSum .&&. conjoin (zipWith checkSum kids resKids)