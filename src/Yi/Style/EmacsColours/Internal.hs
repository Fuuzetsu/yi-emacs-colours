{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      : Yi.Style.EmacsColours.Internal
-- License     : GPL-2
-- Copyright   : © Mateusz Kowalczyk, 2014
-- Maintainer  : fuuzetsu@fuuzetsu.co.uk
-- Stability   : experimental
-- Portability : portable
--
-- Internal-use module.

module Yi.Style.EmacsColours.Internal where

import           Data.Bits (shiftR)
import           Data.Char (toLower, toUpper, isSpace)
import           Data.List (nub)
import           Data.List.Split (splitOn)
import qualified Data.Map as M
import           Data.Maybe (catMaybes)
import           Data.Word (Word32)
import           Text.Read (readMaybe)
import           Yi.Style (Color(RGB))

-- | Convenience function
rgb :: Word32 -> Color
rgb x = RGB (fromIntegral (x `shiftR` 16))
            (fromIntegral (x `shiftR` 8))
            (fromIntegral x)

-- | Temporary type to carry colour info we need for conversion to
-- string
data ColInfo = ColInfo { _colRgb :: (String, String, String)
                       , _colHex :: (String, Color)
                       , _colNames :: [String]
                       } deriving (Eq, Show)

-- | Takes a string that looks like this:
--
-- @
-- 255 250 250	#fffafa	snow	snow
-- 248 248 255	#f8f8ff	ghost white	ghost white
-- 248 248 255	#f8f8ff	GhostWhite	GhostWhite
-- @
--
-- and massages it to spit out a ready-to-go series of functions. Make
-- sure to add the imports and module headers and just splice in the
-- rest.
--
-- You might use it as
--
-- @readFile "/tmp/colors" >>= writeFile "/tmp/colout" . toHaskell@
toHaskell :: String -> String
toHaskell xs =
  let ns = catMaybes $ map mkCol (lines xs)
      as = ms $ map (\ci -> (mkFn $ _colNames ci, ci)) ns
      ms [] = []
      ms ((Nothing, _):ys) = ms ys
      ms ((Just y, c):ys) = (y, c) : ms ys
      jn ci ci' = ci { _colNames = _colNames ci ++ _colNames ci' }
      mp = M.fromListWith jn as

  in unlines $ map (uncurry colToHaskell) (M.toList mp)

-- | Coerce colour info into Haskell function thing
colToHaskell :: String -> ColInfo -> String
colToHaskell fn (ColInfo (r, g, b) (n, h) ns) = splice
  where
    mkRgb = "R" ++ r ++ " G" ++ g ++ " B" ++ b
    mkName = "Names: @" ++ show (nub ns) ++ "@"

    splice = unlines $
      [ "-- | " ++ mkName
      , "--"
      , "-- " ++ mkRgb ++ ", " ++ n
      , fn ++ " :: Color"
      , fn ++ " = " ++ show h
      ]

-- | Massage first possible name into Haskell function name
mkFn :: [String] -> Maybe String
mkFn ns = case catMaybes $ map mkFn' ns of
  [] -> Nothing
  s:_ -> Just s
  where
    mkFn' pn = case dropWhile isSpace pn of
      [] -> Nothing
      s:ss -> Just $ toLower s : camelise ss

    camelise xs = case splitOn " " xs of
      [] -> []
      z:zs -> z ++ concatMap firstUpper zs
    firstUpper [] = []
    firstUpper (c:cs) = toUpper c : cs

-- | Parse out data from a line
mkCol :: String -> Maybe ColInfo
mkCol s = case splitOn "\t" s of
  [rgb', '#':hex', pn', sn'] -> case splitOn " " rgb' of
    [r, g, b] -> case readMaybe $ "0x" ++ hex' of
      Just hex'' ->
        Just $ ColInfo (r, g, b) ("0x" ++ hex', rgb hex'') [pn', sn']
      _ -> Nothing
    _ -> Nothing
  _ -> Nothing
