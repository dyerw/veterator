{-# LANGUAGE NamedFieldPuns #-}

module Display where

import Control.Monad (forM_)
import Display.View (Primitive (..), Px, View (..), layout)
import Foreign.C (CInt (..))
import Linear (V2 (V2))
import Linear.Affine (Point (P))
import Resources (Image (imageTexture), Resources, getImage)
import SDL (WindowConfig (..))
import qualified SDL

type Pos = (Point V2 Int)

pos :: Int -> Int -> Pos
pos x y = P (V2 x y)

initialize :: IO (SDL.Window, SDL.Renderer)
initialize = do
  SDL.initializeAll
  window <- SDL.createWindow "Veterator" SDL.defaultWindow {windowResizable = True}
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  pure (window, renderer)

data Camera = Camera {cameraTranslation :: Px, cameraScale :: Float} deriving (Show)

render :: Resources -> SDL.Renderer -> Camera -> View -> IO ()
render res renderer camera view = do
  SDL.clear renderer
  forM_ (layout view) (renderPrimitive res renderer camera)
  SDL.present renderer

renderPrimitive :: Resources -> SDL.Renderer -> Camera -> (Px, Primitive) -> IO ()
renderPrimitive res renderer Camera {cameraScale, cameraTranslation} (worldPos, prim) = case prim of
  SpritePrim imgKey -> do
    let texture = imageTexture (getImage imgKey res)
    let (V2 x y) = worldPos + cameraTranslation
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