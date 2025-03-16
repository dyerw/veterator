module Resources where

import qualified SDL
import SDL.Font (Font)
import qualified SDL.Font as Font
import SDL.Image (loadTexture)

data ImageKey
  = PlayerImage
  | WallTileImage
  | FloorTileImage
  | GoblinImage
  | TilesSpriteSheet
  | RoguesSpriteSheet
  | MonstersSpriteSheet
  deriving (Show)

data Resources = Resources
  { playerImage :: Image,
    wallTileImage :: Image,
    floorTileImage :: Image,
    goblinImage :: Image,
    tilesSpriteSheet :: Image,
    roguesSpriteSheet :: Image,
    monstersSpriteSheet :: Image,
    cozetteFont :: Font
  }

newtype Image = Image {imageTexture :: SDL.Texture}

loadResources :: SDL.Renderer -> IO Resources
loadResources renderer = do
  playerTexture <- loadTexture renderer "data/images/player.png"
  floorTexture <- loadTexture renderer "data/images/floor.png"
  wallTexture <- loadTexture renderer "data/images/wall.png"
  goblinTexture <- loadTexture renderer "data/images/goblin.png"
  tilesTexture <- loadTexture renderer "data/images/32rogues/tiles.png"
  roguesTexture <- loadTexture renderer "data/images/32rogues/rogues.png"
  monstersTexture <- loadTexture renderer "data/images/32rogues/monsters.png"
  cozetteFont' <- Font.load "data/fonts/CozetteVector.ttf" 16
  pure $
    Resources
      { playerImage = Image playerTexture,
        floorTileImage = Image floorTexture,
        wallTileImage = Image wallTexture,
        goblinImage = Image goblinTexture,
        tilesSpriteSheet = Image tilesTexture,
        roguesSpriteSheet = Image roguesTexture,
        monstersSpriteSheet = Image monstersTexture,
        cozetteFont = cozetteFont'
      }

getImage :: ImageKey -> Resources -> Image
getImage PlayerImage = playerImage
getImage WallTileImage = wallTileImage
getImage FloorTileImage = floorTileImage
getImage GoblinImage = goblinImage
getImage TilesSpriteSheet = tilesSpriteSheet
getImage RoguesSpriteSheet = roguesSpriteSheet
getImage MonstersSpriteSheet = monstersSpriteSheet