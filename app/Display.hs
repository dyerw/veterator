module Display where

import Control.Monad (forM_)
import Display.View (Primitive (..), Px, View (..), layout)
import Foreign.C (CInt (..))
import Linear (V2 (V2))
import Linear.Affine (Point (P))
import Resources (Image (imageTexture), Resources, getImage)
import qualified SDL

type Pos = (Point V2 Int)

pos :: Int -> Int -> Pos
pos x y = P (V2 x y)

initialize :: IO (SDL.Window, SDL.Renderer)
initialize = do
  SDL.initializeAll
  window <- SDL.createWindow "Veterator" SDL.defaultWindow
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  pure (window, renderer)

render :: Resources -> SDL.Renderer -> View -> IO ()
render res renderer view = do
  SDL.clear renderer
  forM_ (layout view) (renderPrimitive res renderer)
  SDL.present renderer

renderPrimitive :: Resources -> SDL.Renderer -> (Px, Primitive) -> IO ()
renderPrimitive res renderer (V2 x y, prim) = case prim of
  SpritePrim imgKey -> do
    let texture = imageTexture (getImage imgKey res)
    _ <-
      SDL.copy
        renderer
        texture
        Nothing -- entire Texture
        ( Just
            ( SDL.Rectangle
                (P (V2 (CInt (fromIntegral x)) (CInt (fromIntegral y))))
                (V2 16 16)
            )
        )
    pure ()