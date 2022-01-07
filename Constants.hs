module Constants (
    screenWidth, 
    screenHeight, 
    horizontalTileCount, 
    verticalTileCount, 
    defaultRectSize,
    tilesetPath
) where

screenWidth, screenHeight :: Int
screenWidth = 1280
screenHeight = 900

defaultRectSize :: (Int, Int)
defaultRectSize = (16, 16)

horizontalTileCount, verticalTileCount :: Float
horizontalTileCount = 1280 / 16
verticalTileCount = 900 / 16
tilesetPath = "tileset.png"
