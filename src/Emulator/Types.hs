module Emulator.Types where

import           Data.Word
import qualified Data.Text                     as T


data CellStyle = SimpleStyle {
  -- TODO: enforce 0-7 on these two
  simpleFg :: Word8,
  simpleBg :: Word8,
  -- Whether this is the dim or bold variant
  simpleDim :: Bool
} | XTermStyle {
  -- TODO: enforce 0-255 on these two
  xtermFg :: Word8,
  xTermBg :: Word8
} deriving (Show)

-- This file attempts to capture all of the possible inputs to the terminal
-- state machine.
-- Reference:
-- https://gist.github.com/fnky/458719343aabd01cfb17a3a4f7296797
data TerminalMutation =
  -- Anything we don't care about just goes in here
  Typing T.Text

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
