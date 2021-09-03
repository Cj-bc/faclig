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
import Graphics.Asciiart.Faclig.Types
import qualified Options.Applicative as OPT


-- option parsers {{{
type Args = FilePath

args :: OPT.Parser Args
args = OPT.argument OPT.str $ OPT.metavar "FACLIG_FILE"

opts :: OPT.ParserInfo Args
opts = OPT.info (args OPT.<**> OPT.helper)
        (OPT.fullDesc <> OPT.progDesc "FDS frontend for ASCII ART")
-- }}}

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
            newFace <- liftIO $ updateFace updateShgif
                                           (setShgifTickTo $ d^.left_eye_percent)
                                           (setShgifTickTo $ d^.right_eye_percent)
                                           updateShgif
                                           (setShgifTickTo $ d^.mouth_height_percent)  -- TODO: apply mouthWSize
                                           updateShgif
                                           updateShgif
                                           f
            newCanvas <- liftIO $ toCanvas f
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
    arg <- OPT.execParser opts

    -- Load face
    face <- load arg

    case face of
        Left e -> print e >> exitFailure
        Right f' -> do
            s <- multicastReceiver defaultGroupAddr defaultPortNumber
            chan <- newBChan 10

            -- Thread to receive FaceData
            forkIO $ forever $ do
                facedata <- getFaceData s
                writeBChan chan $ GetFaceData facedata
                threadDelay 1000 -- wait 1 ms

            emptyCanvas <- newCanvas (1, 1)

            let initialState = AppState f' 0 0 0 0 0.0 0.0 0.0 (0,0) (0,0) (0,0) (0, 0) (0, 0) 0
                                                         emptyCanvas Nothing
                buildVty = Vty.mkVty Vty.defaultConfig
            vty <- buildVty
            void $ customMain vty buildVty (Just chan) app initialState
