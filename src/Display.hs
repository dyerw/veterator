module Display where

import Control.Monad (forM_)
import Display.View (AbsoluteView, Color (Color), Primitive (..), Px, TextAlignment (..))
import Foreign.C (CInt (..))
import Linear (V2 (V2), V4 (V4))
import Linear.Affine (Point (P))
import Resources (Image (imageTexture), Resources (cozetteFont), getImage)
import SDL (WindowConfig (..), ($=))
import qualified SDL
import qualified SDL.Font as Font

type Pos = (Point V2 Int)

pos :: Int -> Int -> Pos
pos x y = P (V2 x y)

initialize :: IO (SDL.Window, SDL.Renderer)
initialize = do
  SDL.initializeAll
  window <- SDL.createWindow "Veterator" SDL.defaultWindow {windowResizable = True}
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  pure (window, renderer)

render :: Resources -> SDL.Renderer -> AbsoluteView -> IO ()
render res renderer view = do
  SDL.clear renderer
  forM_ view (renderPrimitive res renderer)
  SDL.present renderer

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

renderPrimitive :: Resources -> SDL.Renderer -> (Px, Primitive) -> IO ()
renderPrimitive res renderer (V2 x y, prim) = case prim of
  TextPrim alignment t -> do
    let font = cozetteFont res

    (width, height) <- Font.size font t
    surface <- Font.blended (cozetteFont res) (V4 255 255 255 255) t
    texture <- SDL.createTextureFromSurface renderer surface

    let (x', y') = case alignment of
          Centered -> (x - div width 2, y)
          LeftAligned -> (x, y)
          RightAligned -> (x - width, y)

    SDL.copy
      renderer
      texture
      Nothing
      (Just $ renderRect x' y' width height)
    pure ()
  SpritePrim imgKey -> do
    let texture = imageTexture (getImage imgKey res)
    _ <-
      SDL.copy
        renderer
        texture
        Nothing -- entire Texture
        (Just $ renderRect x y (16 :: Int) (16 :: Int))
    pure ()
  RectPrim (V2 width height) (Color r g b a) -> do
    SDL.rendererDrawColor renderer $= V4 (fi r) (fi g) (fi b) (fi a)
    _ <-
      SDL.fillRect
        renderer
        (Just $ renderRect x y width height)
    pure ()
  where
    renderRect x' y' width height =
      SDL.Rectangle
        (P (V2 (CInt (fi x')) (CInt (fi y'))))
        (V2 (fi width) (fi height))