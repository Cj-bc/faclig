{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Concurrent (forkIO)
import Control.Lens (makeLenses, (^.), (&), (.~), over, set, _Just, (%~))
import Control.Monad (when, mapM_, void)
import Control.Monad.IO.Class (liftIO)
import System.Exit (exitFailure)
import System.Environment (getArgs)
import qualified Graphics.Vty as Vty
import Brick
import Brick.Types (Location)
import Brick.Widgets.Border (border)
import Brick.BChan
import Brick.Extensions.Shgif.Widgets (canvas)
import Shgif.Type (Shgif,  shgifToCanvas, width, height)
import Shgif.Loader (fromFile)
import Shgif.Updater (setTickTo)
import Tart.Canvas
import Data.Time.Clock (getCurrentTime, UTCTime, diffUTCTime)
import Graphics.Asciiart.Faclig.Types
import qualified Options.Applicative as OPT
import Pipes.VMCP.Marionette (recvMarionetteMsg)
import Pipes
import qualified Pipes.Prelude as P
import Data.VRM
import Data.VMCP.Marionette (MarionetteMsg(..))
import Data.UnityEditor
import qualified Data.HashMap.Strict as M
import Linear.Quaternion (Quaternion(..))
import Linear.V3 (V3(..))
import Data.Default (Default(..))


-- option parsers {{{
type Args = FilePath
type Radian = Double
type Percent = Int

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
data Transform = Transform { _rotation :: Quaternion Float, _position :: V3 Float }

instance Default Transform where
  def = Transform (Quaternion 0 (V3 0 0 0)) (V3 0 0 0)

data AppState = AppState { _face :: Face
                         , _blendShapes :: M.HashMap BlendShapeExpression Float
                         , _faceRotation :: Transform
                         , _tick :: Int
                         , _currentCanvas :: Canvas
                         , _debugInfo :: Maybe DebugInfo
                         }

defaultBlendShapes = M.fromList [ (Neutral, 0), (A , 0), (I , 0), (U , 0), (E , 0), (O, 0)
                                , (Blink, 0), (Joy , 0), (Angry , 0), (Sorrow , 0)
                                , (Fun, 0), (LookUp , 0), (LookDown, 0)
                                , (LookLeft , 0), (LookRight, 0)
                                , (BlinkL , 0), (BlinkR, 0)]
makeLenses ''AppState

data Name = NoName deriving (Eq, Ord)

data CustomEvent = Tick
                 | UpdateBlendShape BlendShapeExpression Float
                 | ApplyBlendShape
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
eHandler s (AppEvent (UpdateBlendShape name val)) = continue $ s&blendShapes%~(M.update (const $ Just val) name)
eHandler s (AppEvent ApplyBlendShape) = do
               let toPercent = round . (* 100)
                   toPercentFlipped x = max 0 $ 100 - (toPercent x)
                   getBshapeVal :: BlendShapeExpression -> Float
                   getBshapeVal name = maybe 0 id $ (s^.blendShapes)M.!?name
               newFace <- liftIO $ updateFace return
                                              (setTickTo . toPercentFlipped $ getBshapeVal BlinkR)
                                              (setTickTo . toPercentFlipped $ getBshapeVal BlinkL)
                                              return
                                              (setTickTo . toPercent $ getBshapeVal O)
                                              return
                                              return
                                              (s^.face)
               new <- liftIO $ toCanvas newFace
               continue . set face newFace
                        . set currentCanvas new
                        $ s
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

convert ::  Pipe MarionetteMsg CustomEvent IO ()
convert = do
  msg <- await
  convert' msg
  convert

convert' :: Monad m => MarionetteMsg -> Pipe MarionetteMsg CustomEvent m ()
convert' (VRMBlendShapeProxyValue name value) = yield $ UpdateBlendShape name value
convert' VRMBlendShapeProxyApply = yield ApplyBlendShape
convert' _ = pure ()

write :: (BChan CustomEvent) -> Consumer CustomEvent IO ()
write chan = loop'
  where
    loop' = do
      m <- await
      lift $ writeBChan chan m
      loop'
      
main :: IO ()
main = do
    arg <- OPT.execParser opts

    -- Load face
    face <- load arg

    case face of
        Left e -> print e >> exitFailure
        Right f' -> do
            chan <- newBChan 10

            forkIO . runEffect $ recvMarionetteMsg "192.168.10.3" 39540
              >-> convert
              >->  write chan

            emptyCanvas <- newCanvas (1, 1)

            let initialState = AppState f' defaultBlendShapes def 0 emptyCanvas Nothing
                buildVty = Vty.mkVty Vty.defaultConfig
            vty <- buildVty
            void $ customMain vty buildVty (Just chan) app initialState
