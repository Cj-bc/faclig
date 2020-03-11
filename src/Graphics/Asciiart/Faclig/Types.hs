{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Asciiart.Faclig.Types
( Face(..)
) where

import Control.Lens (makeLenses, (^.), _1, set)
import Shgif.Type (Shgif,  shgifToCanvas, width, height)
import Data.Yaml
import Data.HashMap.Lazy ((!))
import Graphics.Asciiart.Faclig.Types.Internal
import Tart.Canvas



-- | A 'Face' represents one face AA model.
--
data Face = Face { _contour :: Part  -- ^ Contour. Basement of face
                 , _leftEye :: Part  -- ^ This is on the right side on the screen.
                 , _rightEye :: Part -- ^ This is on the left side on the screen.
                 , _nose  :: Part
                 , _mouth :: Part
                 , _hair :: Part
                 , _backHair :: Part
                 }
makeLenses ''Face



-- | Render Canvas into other canvas
plotToCanvas :: (Int, Int) -> Canvas -> Canvas -> IO Canvas
plotToCanvas (dw, dh) bc c = do
    let (w, h) = canvasSize c
    write [(w', h') | w' <- [0..w-1], h' <- [0..h-1]] bc
    where
        write :: [(Int, Int)] -> Canvas -> IO Canvas
        write [] bc'         = return bc'
        write ((w, h):x) bc' = do
            let (ch, attr) = canvasGetPixel c (w, h)
            case ch of
                ' ' -> write x bc
                _   -> do
                  newC <- canvasSetPixel bc (w + dw, h + dh) ch attr
                  write x newC


-- | Merge and render all Shgifs into one Canvas
mergeToBigCanvas :: [(Shgif, (Int, Int))] -> IO Canvas
mergeToBigCanvas ss = do
    emptyCanvas <- newCanvas (w, h)
    write ss emptyCanvas
    where
        w = maximum $ fmap (\(s, (w',_)) -> s^.width + w') ss
        h = maximum $ fmap (\(s, (_,h')) -> s^.height + h') ss

        -- | Write Canvases one after another
        write :: [(Shgif, (Int, Int))] -> Canvas -> IO Canvas
        write [] c         = return c
        write ((s,p):x) bc = do
            shgifC <- shgifToCanvas s
            newC <- plotToCanvas p bc shgifC
            write x newC
