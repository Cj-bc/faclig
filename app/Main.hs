{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Lens (makeLenses, (^.), (&), (.~), over, set)
import Control.Monad (when, forM_, forever, void)
import Control.Monad.IO.Class (liftIO)
import System.Exit (exitFailure, exitSuccess)
import System.Environment (getArgs)
import Data.Either (isLeft)
import qualified Graphics.Vty as Vty
import Brick
import Brick.Types (Location)
import Brick.Widgets.Border (border)
import Brick.BChan
import Brick.Extensions.Shgif.Widgets (shgif, canvas)
import Shgif.Type (Shgif,  shgifToCanvas, width, height)
import Shgif.Loader (fromFile)
import Shgif.Updater (updateShgifNoLoop, updateShgif, updateShgifReversedNoLoop
                     , setShgifTickTo)
import FaceDataServer
import FaceDataServer.Types
import FaceDataServer.Connection (getFaceData)
import Tart.Canvas
import Network.Multicast (multicastReceiver)
import Data.Time.Clock (getCurrentTime, UTCTime, diffUTCTime)

helpText = unlines ["faclig -- prototype program to do live2d like animation with shgif"
                   , ""
                   , "Key control:"
                   , "    q: quit program"
                   , "    w: switch right eye"
                   , "    e: switch left eye"
                   , "    m: switch mouth"
                   , "    l: look left"
                   , "    h: look right"
                   ]

-- data types {{{
data Face = Face { _contour :: Shgif
                 , _leftEye :: Shgif
                 , _rightEye :: Shgif
                 , _nose  :: Shgif
                 , _mouth :: Shgif
                 , _hair :: Shgif
                 , _backHair :: Shgif
                 }
makeLenses ''Face

-- data types {{{
data DebugInfo = DebugInfo { _lastFrameArrivedTime :: UTCTime
                           , _fps :: Int
                           }
makeLenses ''DebugInfo

emptyDebugInfo = DebugInfo <$> getCurrentTime
                           <*> pure 0

data AppState = AppState { _face :: Face
                         , _rightEyeSize :: Percent
                         , _leftEyeSize :: Percent
                         , _mouthWSize :: Percent
                         , _mouthHSize :: Percent
                         , _faceXRotation :: Radian
                         , _faceYRotation :: Radian
                         , _faceZRotation :: Radian
                         , _rightEyeOffset :: (Int, Int)
                         , _leftEyeOffset :: (Int, Int)
                         , _mouthOffset :: (Int, Int)
                         , _hairOffset :: (Int, Int)
                         , _noseOffset :: (Int, Int)
                         , _tick :: Int
                         , _currentCanvas :: Canvas
                         , _debugInfo :: Maybe DebugInfo
                         }
makeLenses ''AppState

data Name = NoName deriving (Eq, Ord)

data CustomEvent = Tick
                 | GetFaceData FaceData
-- }}}

-- UI {{{
-- | Render face
ui :: AppState -> [Widget Name]
ui s = [ translateBy (Location (50, 0)) . debugUI $ s^.debugInfo
       , canvas [s^.currentCanvas]
       ]

debugUI :: Maybe DebugInfo -> Widget Name
debugUI Nothing  = emptyWidget
debugUI (Just i) = border . str $ "FPS: " ++ show (i^.fps)
-- }}}

-- event handler {{{
-- | event handler
eHandler :: AppState -> BrickEvent name CustomEvent -> EventM Name (Next AppState)
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'q') [])) = halt s
eHandler s (AppEvent (GetFaceData d)) = do
            -- TODO: Use mouth_width_percent, face_x_radian, face_y_radian, face_z_radian
            let f = s^.face
            newFace <- liftIO $ Face <$> updateShgif (f^.contour)
                                     <*> setShgifTickTo (d^.left_eye_percent)  (f^.leftEye)
                                     <*> setShgifTickTo (d^.right_eye_percent) (f^.rightEye)
                                     <*> updateShgif (f^.nose)
                                     <*> setShgifTickTo (d^.mouth_height_percent) (f^.mouth) -- TODO: apply mouthWSize
                                     <*> updateShgif (f^.hair)
                                     <*> updateShgif (f^.backHair)
            newCanvas <- liftIO $ mergeToBigCanvas [ (f^.hair    , (5, 0))
                                                   , (f^.rightEye, (13, 15))
                                                   , (f^.leftEye , (29, 15))
                                                   , (f^.nose    , (25, 20))
                                                   , (f^.mouth   , (22, 24))
                                                   , (f^.contour , (0, 0))
                                                   , (f^.backHair, (4, 0))
                                                   ]
            debugInfo' <- updateDebugInfo (s^.debugInfo)
            continue . set face newFace
                     . set currentCanvas newCanvas
                     . set debugInfo debugInfo'
                     $ s
    where
        -- | Update Debug Information if it required
        updateDebugInfo :: Maybe DebugInfo -> EventM Name (Maybe DebugInfo)
        updateDebugInfo Nothing  = return Nothing
        updateDebugInfo (Just i) = do
            currentTime <- liftIO getCurrentTime
            let diffSec = diffUTCTime currentTime (i^.lastFrameArrivedTime)
                c_fps   = round $ 1 / diffSec

            return . Just . set lastFrameArrivedTime currentTime
                          . set fps c_fps
                          $ i


eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'i') [])) = case s^.debugInfo of
                                                        Nothing -> do
                                                            di <- liftIO emptyDebugInfo
                                                            continue $ set debugInfo (Just di) s
                                                        Just _ -> continue $ set debugInfo Nothing s
eHandler s _ = continue s

_moveOffsetTo :: (Int, Int) -> (Int, Int) -> (Int, Int)
_moveOffsetTo limit@(x1, y1) current@(x2, y2) | current == limit = current
                                              | x1 == x2         = updateY current
                                              | y1 == y2         = updateX current
                                              | otherwise        = updateX $ updateY current
    where
        updateX (tx, ty) = if x2 < x1
                             then (tx + 1, ty)
                             else (tx - 1, ty)
        updateY (tx, ty) = if y2 < y1
                             then (tx, ty + 1)
                             else (tx, ty - 1)
-- }}}

-- Utilities for Canvas {{{

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
-- }}}






app :: App AppState CustomEvent Name
app = App { appDraw         = ui
          , appHandleEvent  = eHandler
          , appStartEvent   = return
          , appChooseCursor = neverShowCursor
          , appAttrMap      = const $ attrMap Vty.defAttr []
          }

main :: IO ()
main = do
    -- help message
    arg <- getArgs
    when (arg /= [] && (head arg == "--help" || head arg == "-h")) $ putStrLn helpText >> exitSuccess

    -- Load resources
    e_hair <- fromFile "resources/shgif/hair.yaml"
    e_contour <- fromFile "resources/shgif/contour.yaml"
    e_leftEye <- fromFile "resources/shgif/leftEye.yaml"
    e_rightEye <- fromFile "resources/shgif/rightEye.yaml"
    e_nose <- fromFile "resources/shgif/nose.yaml"
    e_mouth <- fromFile "resources/shgif/mouth.yaml"
    e_backHair <- fromFile "resources/shgif/hair_back.yaml"

    -- validate if all resources are loaded correctly
    let fromLeft (Left e) = e
    forM_ [e_hair, e_contour, e_leftEye, e_rightEye, e_nose, e_mouth, e_backHair] $ \e ->
        when (isLeft e) $ print (fromLeft e) >> exitFailure

    -- Unpack Either and construct face
    let (Right c)  = e_contour
        (Right le) = e_leftEye
        (Right re) = e_rightEye
        (Right ns) = e_nose
        (Right m)  = e_mouth
        (Right h)  = e_hair
        (Right hb) = e_backHair
        face       = (Face c le re ns m h hb)

    s <- multicastReceiver defaultGroupAddr defaultPortNumber
    chan <- newBChan 10

    -- Thread to receive FaceData
    forkIO $ forever $ do
        facedata <- getFaceData s
        writeBChan chan $ GetFaceData facedata
        threadDelay 1000 -- wait 1 ms

    emptyCanvas <- newCanvas (1, 1)

    let initialState = AppState face 0 0 0 0 0.0 0.0 0.0 (0,0) (0,0) (0,0) (0, 0) (0, 0) 0
                                                 emptyCanvas Nothing
        buildVty = Vty.mkVty Vty.defaultConfig
    void $ customMain buildVty (Just chan) app initialState
