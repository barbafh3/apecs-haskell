module Engine.Constants (
    screenWidth, 
    screenHeight, 
    horizontalTileCount, 
    verticalTileCount, 
    tileSize,
    tileSizeF,
    defaultRectSize,
    defaultRectSizeV2,
    tilesetPath,
    whiteFontPath,
    blackFontPath,
    fontCharSize,
    targetFps,
    haulerCapacity
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

fontCharSize :: (Int, Int)
fontCharSize = (10, 16)

tilesetPath = "Assets/tileset.png"
whiteFontPath = "Assets/my-font-black.png"
blackFontPath = "Assets/my-font-white.png"

haulerCapacity :: Float
haulerCapacity = 10.0
