{-# LANGUAGE OverloadedStrings #-}
module Emulator.Parser
  ( parseOutput
  )
where

import           Text.Parsec
import           Text.Parsec.Text
import           Control.Monad
import qualified Data.Text                     as T

import           Emulator.Types

data CommonCode = CommonCode [Int] Char

translateCode :: CommonCode -> TerminalMutation
translateCode (CommonCode []      'A') = CursorUpLines 1
translateCode (CommonCode [count] 'A') = CursorUpLines count
translateCode (CommonCode []      'B') = CursorDownLines 1
translateCode (CommonCode [count] 'B') = CursorUpLines count
translateCode (CommonCode []      'C') = CursorRightCols 1
translateCode (CommonCode [count] 'C') = CursorRightCols count
translateCode (CommonCode []      'D') = CursorLeftCols 1
translateCode (CommonCode [count] 'D') = CursorLeftCols count
translateCode (CommonCode []      'H') = CursorHome
translateCode (CommonCode []      's') = SaveCursor
translateCode (CommonCode []      'u') = RestoreCursor
translateCode (CommonCode []      'J') = ClearScreen
translateCode (CommonCode [0]     'J') = ClearCursorEnd
translateCode (CommonCode [1]     'J') = ClearLineCursorBeginning
translateCode (CommonCode [2]     'J') = ClearEntireScreen
translateCode (CommonCode []      'K') = ClearLineCursorEnd
translateCode (CommonCode [0]     'K') = ClearLineCursorEnd
translateCode (CommonCode [1]     'K') = ClearLineCursorBeginning
translateCode (CommonCode [2]     'K') = ClearEntireLine
translateCode _                        = Unparsed

commonCode = do
  string "\ESC["
  optionMaybe (char '?')
  params <- sepBy ansiNum (char ';')
  c      <- letter
  return $ CommonCode (map read params) c

ansiNum = many digit

command =
  try (commonCode >>= return . translateCode)
    <|> (satisfy (/= '\ESC') >>= return . Raw . T.singleton)

parseOutput :: T.Text -> Either ParseError [TerminalMutation]
parseOutput text = parse (many command) "(unknown)" text
