{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens (makeLenses, (^.), (&), (.~), over, set)
import Control.Concurrent (forkIO)
import Control.Monad (when, mapM_, void)
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
import Tart.Canvas
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
            chan <- newBChan 10

            -- Thread to receive FaceData

            emptyCanvas <- newCanvas (1, 1)

            let initialState = AppState f' 0 0 0 0 0.0 0.0 0.0 (0,0) (0,0) (0,0) (0, 0) (0, 0) 0
                                                         emptyCanvas Nothing
                buildVty = Vty.mkVty Vty.defaultConfig
            vty <- buildVty
            void $ customMain vty buildVty (Just chan) app initialState
