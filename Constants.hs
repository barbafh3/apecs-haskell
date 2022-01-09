module Constants (
    screenWidth, 
    screenHeight, 
    horizontalTileCount, 
    verticalTileCount, 
    tileSize,
    tileSizeF,
    defaultRectSize,
    defaultRectSizeV2,
    tilesetPath,
    targetFps
) where
import Linear (V2(V2))

screenWidth, screenHeight :: Int
screenWidth = 1280
screenHeight = 900

targetFps :: Int
targetFps = 60

tileSize = 16
tileSizeF = 16.0

defaultRectSize :: (Int, Int)
defaultRectSize = (tileSize, tileSize)
defaultRectSizeV2 :: V2 Float
defaultRectSizeV2 = V2 tileSizeF tileSizeF

horizontalTileCount, verticalTileCount :: Float
horizontalTileCount = 1280 / 16
verticalTileCount = 900 / 16
tilesetPath = "tileset.png"
