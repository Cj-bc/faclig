{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Asciiart.Faclig.Types.Internal where


import Control.Lens (makeLenses, (^.))
import Shgif.Type (Shgif)
import Data.Yaml
import Data.HashMap.Lazy ((!))

type Offset = (Int, Int)
type Part = (Shgif, Offset)
type UnloadedPart = (FilePath, Offset)

-- For internal {{{

-- | 'Face' that holds 'Shgif.Types.Shgif' as 'FilePath'.
--
-- This type aims to hold data while loading 'Face'.
-- Only for internal use.
data FaceFile = FaceFile { _fContour  :: UnloadedPart
                         , _fLeftEye  :: UnloadedPart
                         , _fRightEye :: UnloadedPart
                         , _fNose     :: UnloadedPart
                         , _fMouth    :: UnloadedPart
                         , _fHair     :: UnloadedPart
                         , _fBackHair :: UnloadedPart
                         }
makeLenses ''FaceFile


instance FromJSON FaceFile where
    parseJSON = withObject "Face" $ \v ->
                    FaceFile <$> parsePart (v ! "contour")
                             <*> parsePart (v ! "leftEye")
                             <*> parsePart (v ! "rightEye")
                             <*> parsePart (v ! "nose")
                             <*> parsePart (v ! "mouth")
                             <*> parsePart (v ! "hair")
                             <*> parsePart (v ! "backHair")

parsePart :: Value -> Parser UnloadedPart
parsePart = withObject "Part" $ \v -> (,) <$> v .: "path"
                                          <*> v .: "offset"



