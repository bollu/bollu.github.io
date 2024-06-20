{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
import Data.Traversable
import Control.Monad

data Cond a = CondGt a a
data Point = Point Float Float

instance Num Point where
  (Point x y) + (Point x' y') = Point (x + x') (y + y')
  (Point x y) - (Point x' y') = Point (x - x') (y - y')
  (Point x y) * (Point x' y') = Point (x * x') (y * y')
  fromInteger x = Point (fromInteger x) (fromInteger x)

newtype Radius = Radius Float deriving(Num)
data Guid = Guid String deriving(Eq, Show)
data Renderable = Circle Guid Point Radius | Line Guid Point Point

data Property where
  CircleRadius :: Property
  CircleCenter :: Property
  LineBegin :: Property
  LineEnd :: Property

data Val = ValPoint Point | ValRadius Radius

data Duration = Sec Float

-- LIBRARY
data Transition =
 Stagger { trduration :: Duration, trts :: [Transition] }
 | SlowEnd { trduration :: Duration, trprop :: Property, trval :: Val, trguid :: Renderable } 
 | Wait { trduration :: Duration }
 | Sequence {trfirst :: Transition, trnext :: Transition}

-- create :: Renderable -> M Guid; create = undefined
-- start :: Transition a -> M (); start = undefined
-- waitforall :: M (); waitforall = undefined


-- growLineFromTo :: Guid -> Transition a
-- growLineFromTo l = undefined

-- APPLICATION
-- gridpts_ :: [Renderable]
-- gridpts_ = [Circle (Point cx cy) (Radius 0) | cx <- [0,10..100], cy <- [0,10..100]]
-- 
-- polypts_ :: [Point]
-- polypts_ = [Point 100 200, Point 200 300, Point 100 200]
-- 
-- centroid :: [Point] -> Point
-- centroid ps = sum ps -- todo fix
-- 
-- barrier :: Point -> Float; barrier = undefined
-- 
-- stepInteriorPt = undefined
-- 
-- iterates :: (a -> Bool) -> (a -> a) -> a -> [a]
-- iterates p f a = case p a of False -> []; True -> a:iterates p f (f a)

{-
data M a = M { mval :: a, mstagenum :: Int }
instance Functor M where 
instance Applicative M where
instance Monad M where 
anim :: M ()
anim = do
  gridpts <- forM gridpts_ create
  lines <- forM (zipWith Line polypts_ (tail polypts_)) create
  potentialpt <- create (Circle (centroid polypts_) (Radius 0))

  start $ Stagger (Sec 0.2) [SlowEnd (Sec 0.5) CircleRadius (Radius 1) pt | pt <- gridpts]
  waitforall

  start $ Onebyone (map growLineFromTo lines)
  waitforall

  start $ SlowEnd (Sec 0.5) CircleRadius (Radius 1) potentialpt
  waitforall

  let locs = iterates (\x -> barrier x > 100) stepInteriorPt (centroid polypts_)
  start (Onebyone [SlowEnd (Sec 0.5) CircleCenter loc potentialpt | loc <- locs])
-}

onebyone :: [Transition] -> Transition;
onebyone [x] = x
onebyone (x:xs) = Sequence x (onebyone xs)

animpingpong =
  let left = Point 50 100; right = Point 300 100;
      circ = Circle (Guid "c") left  (Radius 0)
      grow = SlowEnd (Sec 0.5) CircleRadius (ValRadius . Radius $ 1) circ
      shrink = SlowEnd (Sec 0.5) CircleRadius (ValRadius . Radius $ 0) circ
      goleft = SlowEnd (Sec 0.5) CircleCenter (ValPoint left) circ
      wait =  Wait (Sec 0.5)
      goright = SlowEnd (Sec 0.5) CircleCenter (ValPoint right) circ
  in onebyone [grow, goleft, wait, goright, shrink]


  
main :: IO ()
main = putStrLn "foo" 
