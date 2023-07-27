{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes          #-}
module Main
  ( main
  ) where

import           HGeometry.Box
import           HGeometry.Point
import           HGeometry.Svg
import           Layouter
import qualified Miso
import           Miso.Svg
import           System.OsPath

--------------------------------------------------------------------------------

myDrawing :: Miso.View action
myDrawing = svg_ [ height_ "600", width_ "800" ]
                 [ draw (Rectangle (Point2 10 20) (Point2 100 (200 :: Int)))
                        [stroke_ "green" ]
                 ]

main :: IO ()
main = do renderSvgToFile [osp|/tmp/out.svg|] myDrawing
