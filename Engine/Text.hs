module Engine.Text(drawText, filterText, filterChar) where

import Apecs.Gloss
import Linear
import Graphics.Gloss
import Engine.Constants (defaultRectSize, defaultRectSizeV2, fontCharSize)

-- import Apecs.Gloss
-- import Linear (V2(V2))
-- import Apecs.Physics.Gloss (Picture(Bitmap))
-- import Graphics.Gloss (Rectangle(Rectangle))
-- import Engine.Constants (defaultRectSize)

fontCharacters = [
    (0, 'a'),
    (1, 'b'),
    (2, 'c'),
    (3, 'd'),
    (4, 'e'),
    (5, 'f'),
    (6, 'g'),
    (7, 'h'),
    (8, 'i'),
    (9, 'j'),
    (10, 'k'),
    (11, 'l'),
    (12, 'm'),
    (13, 'n'),
    (14, 'o'),
    (15, 'p'),
    (16, 'q'),
    (17, 'r'),
    (18, 's'),
    (19, 't'),
    (20, 'u'),
    (21, 'v'),
    (22, 'w'),
    (23, 'x'),
    (24, 'y'),
    (25, 'z'),
    (26, '1'),
    (27, '2'),
    (28, '3'),
    (29, '4'),
    (30, '5'),
    (31, '6'),
    (32, '7'),
    (33, '8'),
    (34, '9'),
    (35, '0'),
    (36, 'A'),
    (37, 'B'),
    (38, 'C'),
    (39, 'D'),
    (40, 'E'),
    (41, 'F'),
    (42, 'G'),
    (43, 'H'),
    (44, 'I'),
    (45, 'J'),
    (46, 'K'),
    (47, 'L'),
    (48, 'M'),
    (49, 'N'),
    (50, 'O'),
    (51, 'P'),
    (52, 'Q'),
    (53, 'R'),
    (54, 'S'),
    (55, 'T'),
    (56, 'U'),
    (57, 'V'),
    (58, 'W'),
    (59, 'X'),
    (60, 'Y'),
    (61, 'Z'),
    (62, '='),
    (63, '|'),
    (64, '/'),
    (65, '('),
    (66, ')'),
    (67, '['),
    (68, ']'),
    (69, '{'),
    (70, '}'),
    (71, '+'),
    (72, '-'),
    (73, '*'),
    (74, ':'),
    (75, ','),
    (76, '.'),
    (77, ' '),
    (78, '?'),
    (79, '\''),
    (80, '"'),
    (81, '<'),
    (82, '>')]

filterChar :: Char -> [(Int, Char)] -> Int
filterChar _ [] = 52
filterChar char [letter] = if char == snd letter
                     then fst letter
                     else 52
filterChar char (letter : ls) = if char == snd letter
                     then fst letter
                     else filterChar char ls

filterText :: String -> [Int]
filterText [] = []
filterText [c] = [filterChar c fontCharacters]
filterText (c:cs) = filterChar c fontCharacters : filterText cs

drawText :: V2 Float -> Picture -> [Int] -> Picture
drawText (V2 x y) (Bitmap font) [i] = translate x y $ BitmapSection (Rectangle (i * fst fontCharSize, 0) fontCharSize) font
drawText pos@(V2 x y) font'@(Bitmap font) (i : is) = Pictures [
            translate x y $ BitmapSection (Rectangle (i * fst fontCharSize, 0) fontCharSize) font,
            drawText (V2 (x + 10) y) font' is]
drawText _ _ _ = blank
