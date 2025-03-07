{-# LANGUAGE NamedFieldPuns #-}

-- | Declarative descriptions of game visuals
module Display.View where

import Data.Text (Text)
import Linear.V2 (V2 (V2))
import Resources (ImageKey)

type Px = V2 Int

data View
  = Group [View]
  | Translate Px View
  | Sprite ImageKey
  | Label Text
  deriving (Show)

data Primitive = SpritePrim ImageKey | TextPrim Text

-- An AbsoluteView is an ordered list of primitives paired with their
-- position in terms of world space
type AbsoluteView = [(Px, Primitive)]

layout :: View -> Camera -> AbsoluteView
layout v Camera {cameraTranslation} = layout' cameraTranslation [] v
  where
    layout' :: Px -> AbsoluteView -> View -> AbsoluteView
    layout' offset acc v' = case v' of
      Group vs -> (layout' offset [] =<< vs) <> acc
      Translate translate v'' -> layout' (offset + translate) acc v''
      Sprite imgKey -> (offset, SpritePrim imgKey) : acc
      Label t -> (offset, TextPrim t) : acc

data Camera = Camera
  { cameraTranslation :: Px,
    cameraScale :: Float
  }
  deriving (Show)

type GameView = View

type UIView = View

layoutScreen :: GameView -> UIView -> Camera -> AbsoluteView
layoutScreen gv uiv cam = layout gv cam <> layout uiv (Camera (px 0 0) 0)

px :: Int -> Int -> Px
px = V2

empty :: View
empty = Group []

isEmpty :: View -> Bool
isEmpty (Group []) = True
isEmpty _ = False

translateRight :: Int -> View -> View
translateRight x = Translate (px x 0)

translateLeft :: Int -> View -> View
translateLeft x = Translate (px (-x) 0)

translateDown :: Int -> View -> View
translateDown y = Translate (px 0 y)

translateUp :: Int -> View -> View
translateUp y = Translate (px 0 (-y))

translateMap :: Int -> (Int -> Px) -> [View] -> [View]
translateMap step translateFn views =
  (\(i, v) -> Translate (translateFn i) v) <$> zip [0, step ..] views

hbox :: Int -> [View] -> View
hbox space = Group . translateMap space (px 0)

vbox :: Int -> [View] -> View
vbox space = Group . translateMap space (`px` 0)

gridLayout :: Int -> Int -> [[View]] -> View
gridLayout w h = vbox h . fmap (hbox w)

sparseGridLayout :: Int -> Int -> [((Int, Int), View)] -> View
sparseGridLayout w h views =
  Group
    [ Translate (px (x * w) (y * h)) view
      | ((x, y), view) <- views
    ]