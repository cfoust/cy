module Emulator.Types where

import           Data.Word
import qualified Data.Text                     as T


data CellStyle = CellStyle {
  cellFg :: Word8,
  cellBg :: Word8,
  cellIsDim :: Bool
} deriving (Show)

-- This file attempts to capture all of the possible inputs to the terminal
-- state machine.
-- Reference:
-- https://gist.github.com/fnky/458719343aabd01cfb17a3a4f7296797
-- http://ascii-table.com/ansi-escape-sequences.php
-- https://www.real-world-systems.com/docs/ANSIcode.html
data TerminalMutation =
  -- Anything we don't care about just goes in here
  Raw T.Text
  -- We have no idea what this is
  | Unrecognized

  -- TOP-LEVEL (UNESCAPED) COMMANDS
  ---------------------------------
  | TerminalBell
  | Backspace
  | Tab
  | Newline
  | VerticalTab
  | FormFeed -- NewPage?
  | CarriageReturn
  -- Escape?
  | Delete

  -- CURSOR COMMANDS
  ------------------
  -- Indices are 1-based, starting from (1,1) in the top left
  ------------------
  -- Move the cursor back to (1,1)
  | CursorHome
  | CursorPosition Int Int
  | CursorUpLines Int
  | CursorDownLines Int
  | CursorRightCols Int
  | CursorLeftCols Int
  -- These two move to the beginning of the line upwards regardless of where we
  -- are on the current line
  | CursorUpLinesBeginning Int
  | CursorDownLinesBeginning Int
  | CursorColumn Int
  | SaveCursor
  | RestoreCursor
  | ShowCursor
  | HideCursor

  -- ERASING
  ----------
  | ClearScreen
  -- Clear from the cursor to the end of the screen
  | ClearCursorEnd
  -- Clear from the cursor to the beginning of the screen
  | ClearCursorBeginning
  -- Not really sure how this is different from ClearScreen...
  | ClearEntireScreen
  -- Clear the current line
  | ClearLine
  -- Clear from the cursor to the end of the line
  | ClearLineCursorEnd
  -- Clear from the cursor to the beginning of the line
  | ClearLineCursorBeginning
  -- ClearScreen:ClearEntireScreen::ClearLine:ClearEntireLine?
  | ClearEntireLine

  -- COLOR STUFF
  --------------
  | SetStyle CellStyle
  | ResetStyle
  | SetBold
  | SetDim
  deriving Show
