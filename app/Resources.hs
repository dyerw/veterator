module Resources where

import qualified SDL
import SDL.Image (loadTexture)

data ImageKey = PlayerImage | WallTileImage | FloorTileImage | GoblinImage

data Resources = Resources
  { playerImage :: Image,
    wallTileImage :: Image,
    floorTileImage :: Image,
    goblinImage :: Image
  }

newtype Image = Image {imageTexture :: SDL.Texture}

loadResources :: SDL.Renderer -> IO Resources
loadResources renderer = do
  playerTexture <- loadTexture renderer "data/player.png"
  floorTexture <- loadTexture renderer "data/floor.png"
  wallTexture <- loadTexture renderer "data/wall.png"
  goblinTexture <- loadTexture renderer "data/goblin.png"
  pure $
    Resources
      { playerImage = Image playerTexture,
        floorTileImage = Image floorTexture,
        wallTileImage = Image wallTexture,
        goblinImage = Image goblinTexture
      }

getImage :: ImageKey -> Resources -> Image
getImage PlayerImage = playerImage
getImage WallTileImage = wallTileImage
getImage FloorTileImage = floorTileImage
getImage GoblinImage = goblinImage