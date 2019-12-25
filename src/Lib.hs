{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
module Lib where

import           Algorithms.Geometry.WeightedClosestPair.Naive
import           Control.Lens hiding (use)
import qualified Data.ByteString.Lazy as B
import           Data.Ext
import           Data.Geometry.Ball
import           Data.Geometry.Point
import           Data.Geometry.Vector
import qualified Data.List as List
import           Data.Maybe (mapMaybe)
import           Data.Range
import           Data.UnBounded
import           Data.Util
import           Text.Blaze (ToMarkup(..), ToValue(..), (!))
import qualified Text.Blaze.Internal as Blaze
import           Text.Blaze.Renderer.Utf8 (renderMarkup)
import           Text.Blaze.Svg
import qualified Text.Blaze.Svg11 as Svg
import qualified Text.Blaze.Svg11.Attributes as A

--------------------------------------------------------------------------------

type Color = String
type Id = Int

data DiskData = DD { _diskId      :: !Int
                   , _attrs       :: [Svg.Attribute]
                   }

makeLenses ''DiskData

instance Eq DiskData where
  (DD i _) == (DD j _) = i == j
instance Show DiskData where
  show (DD i _) = "DD " <> show i
instance Semigroup DiskData where
  l <> _ = l

sq x = x*x

myDisks :: [Disk () Double :+ DiskData]
myDisks = [ Disk (ext $ Point2 100 70) (sq 1) :+ (DD 1 attrs)
          , Disk (ext $ Point2 200 170) (sq 1) :+ (DD 2 attrs)
          , Disk (ext $ Point2 500 220) (sq 2) :+ (DD 2 attrs)
          , Disk (ext $ Point2 700 470) (sq 1.5) :+ (DD 2 attrs)
          , Disk (ext $ Point2 600 430) (sq 4) :+ (DD 2 attrs)
          , Disk (ext $ Point2 1100 190) (sq 5) :+ (DD 2 attrs)
          , Disk (ext $ Point2 1440 450) (sq 1.5) :+ (DD 2 attrs)
          , Disk (ext $ Point2 1300 30) (sq 2.1) :+ (DD 2 attrs)
          , Disk (ext $ Point2 110 430) (sq 0.3) :+ (DD 2 attrs)
          -- , Disk (ext $ Point2 680 60) (sq 0.4) :+ (DD 2 attrs)
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


data TimeInterval r = TimeInterval { _startTime :: Bottom r
                                   , _endTime ::  Top r
                                   } deriving (Show,Eq)
makeLenses ''TimeInterval

data RenderedDiskData r = RD { _diskData :: !DiskData
                             , _visibleInterval :: !(TimeInterval r)
                             } deriving (Show,Eq)

makeLenses ''RenderedDiskData

-- | Find the maximum ending time in one of the disks.
lastTime :: Ord r => [core :+ RenderedDiskData r] -> r
lastTime = maximum . mapMaybe (^?extra.visibleInterval.endTime._ValT)


animId i = "trans" <> toValue i

use :: Svg -> Svg
use = Blaze.Parent "use" "<use" "</use>"

circle = Blaze.Parent "circle" "<circle" "</circle>"

vectorEffect = Blaze.attribute "vector-effect" " vector-effect=\""

renderTime t = "global.begin+" <> toValue t <> "s"

-- timeValue   :: Num r => r -> Value
timeValue t = toValue t


-- prdocues a def and the animation
render :: (ToValue r, Floating r) => Disk p r :+ RenderedDiskData r -> Svg
render (Disk (c :+ _) r :+ x) = circle
                                ! A.cx           "0"
                                ! A.cy           "0"
                                ! A.r            (toValue $ sqrt r)
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
    ti@(TimeInterval s t) = x^.visibleInterval

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
                ! A.begin           (renderTime $ case s of Bottom -> 0 ; ValB s'' -> s'')
                -- ! A.id_             (toValue $ x^.diskData.diskId.to animId)
                ! A.type_           "scale"
                ! A.additive        "sum"
                ! A.from            (toValue s)
                ! A.to              (toValue t)
                ! A.dur             (renderInterval ti)
                ! A.fill            "freeze"


renderInterval                    :: (Fractional r, ToValue r)
                                  => TimeInterval r -> Svg.AttributeValue
renderInterval (TimeInterval s t) = timeValue (t' - s') <> "s"
  where
    s' = case s of Bottom -> 0                      ; ValB s'' -> s''
    t' = case t of Top    -> realToFrac maxDuration ; ValT t'' -> t''


instance ToValue Rational where
  toValue = toValue . realToFrac @Rational @Double


instance (ToValue r, Num r) => ToValue (Bottom r) where
  toValue Bottom   = "0"
  toValue (ValB x) = timeValue x

instance (ToValue r, Num r) => ToValue (Top r) where
  toValue (ValT x) = timeValue x
  toValue Top      = timeValue maxDuration


backgroundColor = "hsl(0, 0%, 93%)"

  -- "palegoldenrod"


renderAll disks = Svg.docTypeSvg ! A.width "100%"
                                 ! A.height "500px"
                  $ do
                    Svg.rect ! A.width "100%"
                             ! A.height "100%"
                             ! A.fill backgroundColor
                    global
                    mconcat (map render $ disks)
  where
    global = Svg.svg $ Svg.animate ! A.id_            "global"
                                   ! A.begin          "0;global.end"
                                   ! A.dur            (timeValue (1 + maxDuration) <> "s")
                                   ! A.attributename  "visibility"
                                   ! A.from           "hide"
                                   ! A.to             "hide"


-- <rect>
--     <animate id="o1" begin="0;o1.end" dur="10s"
--     attributeName="visibility" from="hide" to="hide"/>
--   </rect>



-- test = Svg.circle ! A.cx "0" $ do
--          Svg.rect ! A.x "0"


main = B.writeFile "/home/frank/workspace/fstaals_net/figures/hero.svg" . renderMarkup . renderAll $ myRenderedDisks

myRenderedDisks = let ds = zoomingTimes myDisks
                  in map (&extra %~ uncurry RD) $ ds


--------------------------------------------------------------------------------

firstIntersection :: (d ~ []
                     , Ord r, Floating r
                     )
                  => d (Disk p r :+ dd) -> (r, Two (Disk p r :+ dd))
firstIntersection ds = let ValT (t,p) = weightedClosestPair ds
                       in (t, p)

  -- undefined
deleteDisk :: (d ~ []
              , Eq p, Eq r, Eq dd
              ) => Disk p r :+ dd -> d (Disk p r :+ dd)  -> d (Disk p r :+ dd)
deleteDisk = List.delete

deleteDisks      :: (d ~ []
                    , Eq p, Eq r, Eq dd
                    ) => [Disk p r :+ dd] -> d  (Disk p r :+ dd) -> d (Disk p r :+ dd)
deleteDisks ds d = List.foldl' (flip deleteDisk) d ds

-- | Computes for each disk the time interval on which it is alive.
-- Note that there may be new disks added.
zoomingTimes :: (Ord r, Floating r, Semigroup p, Semigroup dd
                , Eq p, Eq dd
                )
             => [Disk p r :+ dd]
             -> [Disk p r :+ (dd, TimeInterval r)]
zoomingTimes = zoomingTimes' . map (&extra %~ (,ValB 0))

-- | Computes for each disk the time interval on which it is alive.
-- Note that there may be new disks added.
zoomingTimes'     :: (Ord r, Floating r, Semigroup p, Semigroup dd
                     , Eq p, Eq dd
                     )
                  => [Disk p r :+ (dd, Bottom r)]
                  -> [Disk p r :+ (dd, TimeInterval r)]
zoomingTimes' []  = []
zoomingTimes' [p] = [p&extra._2 %~ flip TimeInterval Top]
zoomingTimes' ds  = (p&extra._2 %~ flip TimeInterval (ValT t))
                  : (q&extra._2 %~ flip TimeInterval (ValT t))
                  : deads <> existingDisks
  where
    (t,(Two p q)) = firstIntersection ds
    new = merge t p q
    (deads,alives) = collapseIntersecting t new (deleteDisks [p,q] ds)
    existingDisks = zoomingTimes' alives


merge t p q = merge' (p^.core) (q^.core) :+ ( p^.extra._1 <> q^.extra._1
                                            , ValB t
                                            )

merge' (Disk (c :+ cp) rs) (Disk (d :+ dp) ss) = Disk (c' :+ cp <> dp) ws
  where
    r = sqrt rs
    s = sqrt ss
    w  = r + s
    ws = w * w --rs + ss + 2*r*s

    c' = Point $ lerp (r/w) (toVec c) (toVec d)


collapseIntersecting t d0 = go []
  where
    go nonIntersecting []     = ([],d0:nonIntersecting)
    go nonIntersecting (d:ds)
      | intersectAt t d0 d =
          let (deads,rest) = collapseIntersecting t (merge t d0 d) (nonIntersecting <> ds)
          in ((d&extra._2 %~ flip TimeInterval (ValT t)) : deads,rest)
      | otherwise          = go (d:nonIntersecting) ds


intersectAt t p q = let rad d = t * d^.core.radius in
  euclideanDist (p^.core.center.core) (q^.core.center.core) < rad p + rad q


maxDuration :: Double
maxDuration = 20 + lastTime myRenderedDisks



color s = let [r,g,b] = ((*256) . read @Double) <$> words s
          in "rgb(" <> show r <> ", " <> show g <> ", " <> show b <> ")"
