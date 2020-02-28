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
import Brick.Widgets.Border (border)
import Brick.BChan
import Brick.Extensions.Shgif.Widgets (shgif, canvas)
import Shgif.Type (Shgif,  shgifToCanvas, width, height)
import Shgif.Loader (fromFile)
import Shgif.Updater (updateShgifNoLoop, updateShgif, updateShgifReversedNoLoop
                     , updateShgifTo,)
import FaceDataServer
import FaceDataServer.Types
import FaceDataServer.Connection (getFaceData)
import Tart.Canvas
import Network.Multicast (multicastReceiver)

multicastGroupAddr = "226.0.0.1"
portNum = 5032

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
data LR = L | R
data FacialExpression = Normal
                      | ClosingEye LR
                      | OpeningEye LR
                      | ClosingEyes
                      | OpeningEyes
                      | ClosingMouth
                      | OpeningMouth

-- | A data type that represent facial Parts state
data PartState = Opened  -- ^ The part is opened
               | Closed  -- ^ The part is closed
               | Opening -- ^ The part is opening
               | Closing -- ^ The part is closing
               | Emote1
               | Emote2
                deriving (Eq)

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
                         }
makeLenses ''AppState

data Name = NoName deriving (Eq, Ord)

data CustomEvent = Tick
                 | GetFaceData FaceData
-- }}}

-- UI {{{
-- | Render face
ui :: AppState -> [Widget Name]
ui s = [ canvas [(s^.currentCanvas)]
       ]
-- }}}

-- event handler {{{
-- | event handler
--
-- key bindings:
--
-- * 'q' : quit the app
--
-- * 'w' : Open right eye
--
-- * 's' : Close right eye
--
-- * 'x' : Open right eye wide
--
-- * 'e' : Open left eye
--
-- * 'd' : Close left eye
--
-- * 'c' : Open left eye wide
--
-- * 'm' : Open/Close mouth
--
-- * 'l' : Look left
--
-- * 'h' : Look right
--
-- * 'o' : Look front
eHandler :: AppState -> BrickEvent name CustomEvent -> EventM Name (Next AppState)
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'q') [])) = halt s
eHandler s (AppEvent (GetFaceData d)) = continue . set rightEyeSize  (d^.right_eye_percent)
                                                 . set leftEyeSize   (d^.left_eye_percent)
                                                 . set mouthWSize    (d^.mouth_width_percent)
                                                 . set mouthHSize    (d^.mouth_height_percent)
                                                 . set faceXRotation (d^.face_x_radian)
                                                 . set faceYRotation (d^.face_y_radian)
                                                 . set faceZRotation (d^.face_z_radian)
                                                 $ s
eHandler s (AppEvent Tick) = continue =<< liftIO (do
                                                  nf <- newFace
                                                  nc <- updateCanvas
                                                  let updateOffset = if ((s^.tick) `mod` 10) == 0
                                                                       then calculateOffset
                                                                       else id
                                                  return . over tick (+1)
                                                         . updateOffset
                                                         . set face nf
                                                         . set currentCanvas nc
                                                         $ s
                                                 )
      where
        updateTick = over tick (+1)
        f = s^.face
        partUpdate partLens condLens = case s^.condLens of
                                       Closing -> updateShgifNoLoop         $ f^.partLens
                                       Opening -> updateShgifReversedNoLoop $ f^.partLens
                                       _       -> return $ f^.partLens
        newFace = Face <$> (updateShgif $ f^.contour)
                       <*> (updateShgifTo (s^.leftEyeSize)  $ f^.leftEye)
                       <*> (updateShgifTo (s^.rightEyeSize) $ f^.rightEye)
                       <*> (updateShgif $ f^.nose)
                       <*> (updateShgifTo (s^.mouthHSize) $ f^.mouth) -- TODO: apply mouthWSize
                       <*> (updateShgif $ f^.hair)
                       <*> (updateShgif $ f^.backHair)
        calculateOffset = case (s^.faceLooking) of
                            Nothing -> calculateOffset' (0, 0) (0, 0) (0, 0) (0, 0) (0, 0)
                            Just R  -> calculateOffset' (-1, 0) (-2, 0) (-1, 0) (0, 0) (-1, 0)
                            Just L  -> calculateOffset' (2, 0) (1, 0) (1, 0) (0, 0) (1, 0)
        calculateOffset' a b c d e = over rightEyeOffset  (_moveOffsetTo a)
                                     . over leftEyeOffset (_moveOffsetTo b)
                                     . over mouthOffset   (_moveOffsetTo c)
                                     . over hairOffset    (_moveOffsetTo d)
                                     . over noseOffset    (_moveOffsetTo e)
        addOffset (a, b) (c, d) = (a + c, b + d)
        updateCanvas = mergeToBigCanvas [ (f^.hair    , (5, 0)   `addOffset` (s^.hairOffset))
                                        , (f^.rightEye, (13, 15) `addOffset` (s^.rightEyeOffset))
                                        , (f^.leftEye , (29, 15) `addOffset` (s^.leftEyeOffset))
                                        , (f^.nose    , (25, 20) `addOffset` (s^.noseOffset))
                                        , (f^.mouth   , (22, 24) `addOffset` (s^.mouthOffset))
                                        , ((f^.contour), (0, 0))
                                        , (f^.backHair, (4, 0))
                                        ]


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
    flip mapM_ [e_hair, e_contour, e_leftEye, e_rightEye, e_nose, e_mouth, e_backHair] $ \e ->
        when (isLeft e) $ putStrLn (show $ fromLeft e) >> exitFailure

    -- Unpack Either and construct face
    let (Right c)  = e_contour
        (Right le) = e_leftEye
        (Right re) = e_rightEye
        (Right ns) = e_nose
        (Right m)  = e_mouth
        (Right h)  = e_hair
        (Right hb) = e_backHair
        face       = (Face c le re ns m h hb)

    s <- multicastReceiver multicastGroupAddr portNum
    chan <- newBChan 10

    -- Thread to receive FaceData
    forkIO $ forever $ do
        facedata <- getFaceData s
        writeBChan chan $ GetFaceData facedata

    -- Thread to generate Tick Event
    forkIO $ forever $ do
        writeBChan chan Tick
        threadDelay 1000 -- wait 1 ms

    emptyCanvas <- newCanvas (1, 1)

    let initialState = AppState face  Opened Opened Opened
                                                 (0,0) (0,0) (0,0) (0, 0) (0, 0) Nothing 0
                                                 emptyCanvas
        buildVty = Vty.mkVty Vty.defaultConfig
    void $ customMain buildVty (Just chan) app initialState
    return ()
