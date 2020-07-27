{-# LANGUAGE OverloadedStrings #-}
module Emulator.Parser
  ( parseOutput
  )
where

import           Text.Parsec
import           Text.Parsec.Text
import           Control.Monad
import qualified Data.Text                     as T

import           Debug.Trace
import           Emulator.Types
import           Data.Maybe

data CommonCode = CommonCode [Int] Char
  deriving Show

translateStyleCode :: CellStyle -> Int -> CellStyle
translateStyleCode x 1  = x { cellIsBold = True }
translateStyleCode x 22 = x { cellIsBold = False }
translateStyleCode x code
  | code >= 30 && code <= 37 = x { cellFg = fromJust (findColor (code - 30)) }
  | code >= 40 && code <= 47 = x { cellBg = fromJust (findColor (code - 40)) }
translateStyleCode x _ = x

translateStyle :: [Int] -> CellStyle
translateStyle []               = cellDefault
translateStyle [38, 5, x]       = cellDefault { cellFg = XTerm256 x }
translateStyle [48, 5, x]       = cellDefault { cellBg = XTerm256 x }
translateStyle [38, 2, r, g, b] = cellDefault { cellFg = RGB r g b }
translateStyle [48, 2, r, g, b] = cellDefault { cellBg = RGB r g b }
translateStyle codes            = foldl translateStyleCode (cellDefault) codes

translateCode :: CommonCode -> TerminalMutation
translateCode (CommonCode []         'A') = CursorUpLines 1
translateCode (CommonCode [count]    'A') = CursorUpLines count
translateCode (CommonCode []         'B') = CursorDownLines 1
translateCode (CommonCode [count]    'B') = CursorUpLines count
translateCode (CommonCode []         'C') = CursorRightCols 1
translateCode (CommonCode [count]    'C') = CursorRightCols count
translateCode (CommonCode []         'D') = CursorLeftCols 1
translateCode (CommonCode [count]    'D') = CursorLeftCols count
translateCode (CommonCode []         'H') = CursorHome
translateCode (CommonCode [row, col] 'H') = CursorPosition row col
translateCode (CommonCode [25]       'h') = ShowCursor
translateCode (CommonCode [25]       'l') = HideCursor
translateCode (CommonCode []         's') = SaveCursor
translateCode (CommonCode []         'u') = RestoreCursor
translateCode (CommonCode []         'J') = ClearScreen
translateCode (CommonCode [0]        'J') = ClearCursorEnd
translateCode (CommonCode [1]        'J') = ClearLineCursorBeginning
translateCode (CommonCode [2]        'J') = ClearEntireScreen
translateCode (CommonCode []         'K') = ClearLineCursorEnd
translateCode (CommonCode [0]        'K') = ClearLineCursorEnd
translateCode (CommonCode [1]        'K') = ClearLineCursorBeginning
translateCode (CommonCode [2]        'K') = ClearEntireLine
translateCode (CommonCode []         'm') = ResetStyle
translateCode (CommonCode codes      'm') = SetStyle $ translateStyle codes
translateCode x                           = trace (show x) Unrecognized

commonCode = do
  string "\ESC["
  optionMaybe (char '?')
  params <- sepBy ansiNum (char ';')
  c      <- letter
  return $ CommonCode (map read params) c

ansiNum = many1 digit

command =
  try (commonCode >>= return . translateCode)
    <|> (satisfy (/= '\ESC') >>= return . Raw . T.singleton)

parseOutput :: T.Text -> Either ParseError [TerminalMutation]
parseOutput text = parse (many command) "(unknown)" text
