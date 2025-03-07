module Display where

import Control.Monad (forM_)
import Display.View (AbsoluteView, Primitive (..), Px)
import Foreign.C (CInt (..))
import Linear (V2 (V2), V4 (V4))
import Linear.Affine (Point (P))
import Resources (Image (imageTexture), Resources (cozetteFont), getImage)
import SDL (WindowConfig (..))
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

renderPrimitive :: Resources -> SDL.Renderer -> (Px, Primitive) -> IO ()
renderPrimitive res renderer (V2 x y, prim) = case prim of
  TextPrim t -> do
    let font = cozetteFont res
    (width, height) <- Font.size font t
    surface <- Font.blended (cozetteFont res) (V4 255 255 255 255) t
    texture <- SDL.createTextureFromSurface renderer surface
    SDL.copy
      renderer
      texture
      Nothing
      (Just $ renderRect width height)
    pure ()
  SpritePrim imgKey -> do
    let texture = imageTexture (getImage imgKey res)
    _ <-
      SDL.copy
        renderer
        texture
        Nothing -- entire Texture
        (Just $ renderRect (16 :: Int) (16 :: Int))
    pure ()
  where
    renderRect width height =
      SDL.Rectangle
        (P (V2 (CInt (fromIntegral x)) (CInt (fromIntegral y))))
        (V2 (fromIntegral width) (fromIntegral height))