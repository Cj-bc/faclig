{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Asciiart.Faclig.Types
( Face(..)
, load
, toCanvas
, updateFace
) where

import Control.Lens (makeLenses, (^.), _1, set)
import Shgif.Type (Shgif,  shgifToCanvas, width, height)
import Data.Yaml
import Data.HashMap.Lazy ((!))
import Graphics.Asciiart.Faclig.Types.Internal
import Tart.Canvas
import System.FilePath.Posix (takeDirectory)
import System.Directory (withCurrentDirectory)



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

-- ========== Functions for Face {{{


-- | Load 'Face' from 'FilePath'
load :: FilePath -> IO (Either ParseException Face)
load fp = do
    res <- decodeFileEither fp :: IO (Either ParseException FaceFile)
    case res of
        Left e -> return $ Left e
        Right f -> do
            withCurrentDirectory (takeDirectory fp) $ do
                cont <- loadPart $ f^.fContour
                le   <- loadPart $ f^.fLeftEye
                re   <- loadPart $ f^.fRightEye
                n    <- loadPart $ f^.fNose
                m    <- loadPart $ f^.fMouth
                h    <- loadPart $ f^.fHair
                bh   <- loadPart $ f^.fBackHair
                return $ Face <$> cont
                              <*> le
                              <*> re
                              <*> n
                              <*> m
                              <*> h
                              <*> bh


-- | Load 'Shgif.Types.Shgif' for 'Part' and return 'Part'
loadPart :: UnloadedPart -> IO (Either ParseException Part)
loadPart (fn, o) = do
    res <- decodeFileEither fn :: IO (Either ParseException Shgif)
    case res of
        Left e -> return $ Left e
        Right p -> return $ Right (p, o)

type Updater = Shgif -> IO Shgif

-- |
--
-- TODO: PLEASE FIX THIS TYPE
updateFace :: Updater -> Updater -> Updater
                -> Updater -> Updater -> Updater
                -> Updater -> Face -> IO Face
updateFace c le re n m hi bh f = do
    new_c <- c  $ f^.contour._1
    new_l <- le $ f^.leftEye._1
    new_r <- re $ f^.rightEye._1
    new_n <- n  $ f^.nose._1
    new_m <- m  $ f^.mouth._1 -- TODO: apply mouthWSize
    new_h <- hi $ f^.hair._1
    new_b <- bh $ f^.backHair._1
    return . set (contour._1)  new_c
           . set (leftEye._1)  new_l
           . set (rightEye._1) new_r
           . set (nose._1)     new_n
           . set (mouth._1)    new_m
           . set (hair._1)     new_h
           . set (backHair._1) new_b
           $ f


-- | Convert current 'Face' into Canvas
toCanvas :: Face -> IO Canvas
toCanvas f = mergeToBigCanvas [ f^.hair
                              , f^.rightEye
                              , f^.leftEye
                              , f^.nose
                              , f^.mouth
                              , f^.contour
                              , f^.backHair
                              ]



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
