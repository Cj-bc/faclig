{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Asciiart.Faclig.Types.Internal where


import Control.Lens (makeLenses, (^.))
import Control.Monad (when)
import Shgif.Type (Shgif)
import Data.Yaml
import Data.Aeson ((<?>))
import Data.Aeson.Internal (JSONPathElement(Key))
import Data.HashMap.Lazy ((!))
import qualified Data.Vector as V

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
    parseJSON = withObject "Face" $ \v -> do
                    parts <- v .: "parts" <?> Key "parts" :: Parser Object
                    let helper n = parsePart (parts ! n) <?> Key n

                    FaceFile <$> helper "contour"
                             <*> helper "leftEye"
                             <*> helper "rightEye"
                             <*> helper "nose"
                             <*> helper "mouth"
                             <*> helper "hair"
                             <*> helper "backHair"


parsePart :: Value -> Parser UnloadedPart
parsePart = withObject "Part" $ \v -> (,) <$> v .: "path" <?> Key "path"
                                          <*> parseTuple (v ! "offset") <?> Key "offset"

-- | Parse Yaml's List to tuple '(,)'
--
-- First two items are used as value,
-- and others will be just discarded.
parseTuple :: Value -> Parser (Int, Int)
parseTuple = withArray "Offset" $ \v -> do
                when (V.length v /= 2) $ fail "offset should be exactly [x: Int, y: Int]"
                (,) <$> parseJSON (v V.! 0) <*> parseJSON (v V.! 1)
