{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Lib where

import           Control.Lens hiding (use)
import qualified Data.ByteString.Lazy as B
import           Data.Ext
import           Data.Geometry.Ball
import           Data.Geometry.Point
-- import           Data.Range
import           Text.Blaze (ToMarkup(..), ToValue(..), (!))
import qualified Text.Blaze.Internal as Blaze
import           Text.Blaze.Renderer.Utf8 (renderMarkup)
import           Text.Blaze.Svg
import qualified Text.Blaze.Svg11 as Svg
import qualified Text.Blaze.Svg11.Attributes as A
import           Data.UnBounded
--------------------------------------------------------------------------------

type Color = String
type Id = Int

data DiskData = DD { _diskId      :: !Int
                   , _attrs       :: [Svg.Attribute]
                   }

makeLenses ''DiskData


myDisks :: [Disk () Rational :+ DiskData]
myDisks = [ Disk (ext $ Point2 100 100) 50 :+ (DD 1 attrs)
          , Disk (ext $ Point2 200 200) 10 :+ (DD 2 attrs)
          ]
  where
    attrs = [ A.stroke "rgb(228,26,28)"
            , A.fill   "rgb(252,154,154)"
            ]

-- instance ToValue r => ToMarkup (Disk p r :+ DiskData) where
--   toMarkup (Disk (c :+ _) r :+ (DD i sc)) =
--     Svg.circle ! A.id_    (toValue i)
--                ! A.cx     (toValue $ c^.xCoord)
--                ! A.cy     (toValue $ c^.yCoord)
--         anim       ! A.r      (toValue r)
--                ! A.stroke (toValue sc)


data TimeInterval r = TimeInterval (Bottom r) (Top r) deriving (Show,Eq)

data RenderedDiskData r = RD { _diskData :: !DiskData
                             , _visibleInterval :: !(TimeInterval r)
                             } -- deriving (Show,Eq)

makeLenses ''RenderedDiskData


animId i = "trans" <> toValue i

use :: Svg -> Svg
use = Blaze.Parent "use" "<use" "</use>"

circle = Blaze.Parent "circle" "<circle" "</circle>"

vectorEffect = Blaze.attribute "vector-effect" " vector-effect=\""

renderTime t = "global.begin+" <> toValue t <> "s"

-- prdocues a def and the animation
render :: (ToValue r, Num r) => Disk p r :+ RenderedDiskData r -> Svg
render (Disk (c :+ _) r :+ x) = circle
                                ! A.cx           "0"
                                ! A.cy           "0"
                                ! A.r            (toValue $ r)
                                ! A.transform    translate'
                                ! vectorEffect   "non-scaling-stroke"
                                ! A.opacity      "0"
                                -- apply the custom effects:
                                ! (mconcat $ x^.diskData.attrs)
                                $ mconcat [ appear
                                          , animation
                                          , dissapear
                                          ]
  where
    (TimeInterval s t) = x^.visibleInterval

    appear    = Svg.animate
                 ! A.dur           "0.01s"
                 ! A.attributename "opacity"
                 ! A.from           "0"
                 ! A.to             "1"
                 ! A.begin          (renderTime s)
                 ! A.fill           "freeze"
    dissapear = Svg.animate
                 ! A.dur           "0.01s"
                 ! A.attributename "opacity"
                 ! A.from          "1"
                 ! A.to            "0"
                 ! A.begin         (renderTime t)
                 ! A.fill          "freeze"

    translate' = mconcat [ "translate("
                         , toValue $ c^.xCoord
                         , " "
                         , toValue $ c^.yCoord
                         , ")"
                         ]

    animation = Svg.animatetransform
                ! A.attributename   "transform"
                ! A.begin           (renderTime s)
                -- ! A.id_             (toValue $ x^.diskData.diskId.to animId)
                ! A.type_           "scale"
                ! A.additive        "sum"
                ! A.from            "0"
                ! A.to              "1"
                ! A.dur             "10s"
                ! A.fill            "freeze"

instance ToValue Rational where
  toValue = toValue . realToFrac @Rational @Double


instance ToValue r => ToValue (Bottom r) where
  toValue Bottom   = "0"
  toValue (ValB x) = toValue x

instance ToValue r => ToValue (Top r) where
  toValue (ValT x) = toValue x
  toValue Top   = "10"


backgroundColor = "palegoldenrod"


renderAll disks = Svg.docTypeSvg ! A.width "100%"
                                 ! A.height "600px"
                  $ do
                    Svg.rect ! A.width "100%"
                             ! A.height "100%"
                             ! A.fill backgroundColor
                    global
                    mconcat (map render $ disks)
  where
    duration = 11 :: Int -- in secionds

    global = Svg.svg $ Svg.animate ! A.id_            "global"
                                   ! A.begin          "0;global.end"
                                   ! A.dur            (toValue duration <> "s")
                                   ! A.attributename  "visibility"
                                   ! A.from           "hide"
                                   ! A.to             "hide"


-- <rect>
--     <animate id="o1" begin="0;o1.end" dur="10s"
--     attributeName="visibility" from="hide" to="hide"/>
--   </rect>



-- test = Svg.circle ! A.cx "0" $ do
--          Svg.rect ! A.x "0"


main = B.writeFile "/tmp/out.svg" . renderMarkup . renderAll $ myRenderedDisks

myRenderedDisks :: [Disk () Rational :+ RenderedDiskData Rational]
myRenderedDisks =
  [ Disk (ext $ Point2 100 100) 50 :+ (RD (DD 1 attrs) $ TimeInterval Bottom Top)
  , Disk (ext $ Point2 200 200) 10 :+ (RD (DD 2 attrs) $ TimeInterval (ValB 3) (ValT 8))
  ]
  where
    attrs = [ A.stroke "rgb(228,26,28)"
            , A.fill   "rgb(252,154,154)"
            ]












-- -- | Computes for each disk the time interval on which it is alive.
-- -- Note that there may be new disks added.
-- zoomingTimes    :: [Disk p r :+ dd] -> ( [Disk p r :+ (dd, Range r)] -- existing disks
--                                        , [Disk p r :+ Range r]) -- new disks
-- zoomingTimes ds = (existingDisks,newDisks)
--   where
--     existingDisks = undefined
--     newDisks = []
