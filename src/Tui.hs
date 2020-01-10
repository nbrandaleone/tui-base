{-# LANGUAGE OverloadedStrings #-}

module Tui where

import System.Directory
import System.Exit

import Control.Monad.IO.Class
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Cursor.Simple.List.NonEmpty

import Brick.AttrMap
import Brick.Widgets.Border
import Brick.Main
import Brick.Types
import Brick.Util
import Brick.Widgets.Core
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events

tui :: IO ()
tui = do
    initialState <- buildInitialState
    endState <- defaultMain tuiApp initialState
    print endState

data TuiState =
    TuiState { tuiStatePaths :: NonEmptyCursor FilePath }
    deriving (Show, Eq)

type ResourceName = String

tuiApp :: App TuiState e ResourceName
tuiApp =
    App
        { appDraw = drawTui
        , appChooseCursor = showFirstCursor
        , appHandleEvent = handleTuiEvent
        , appStartEvent = pure
        , appAttrMap = const $ attrMap mempty [("selected", fg red)]
        }

buildInitialState :: IO TuiState
buildInitialState = do
    here <- getCurrentDirectory
    contents <- getDirectoryContents here
    case NE.nonEmpty contents of
        Nothing -> die "There are no contents."
        Just ne -> pure TuiState {tuiStatePaths = makeNonEmptyCursor ne }
-- NE.nonEmpty :: [a] -> Maybe (NonEmpty a)

-- vBox :: [Widget n] -> Widget n
drawTui :: TuiState -> [Widget ResourceName]
drawTui ts = 
    let nec = tuiStatePaths ts
      in [ border $
           vBox $
           concat
               [ map (drawPath False) $ reverse $ nonEmptyCursorPrev nec
               , [drawPath True $ nonEmptyCursorCurrent nec]
               , map (drawPath False) $ nonEmptyCursorNext nec
               ]
         ]
--	vBox $ map drawPath $ tuiStatePaths ts

-- str :: String -> Widget n
drawPath :: Bool -> FilePath -> Widget n
drawPath b = 
    (if b
        then withAttr "selected"
        else id) .
    str

handleTuiEvent :: TuiState -> BrickEvent n e -> EventM n (Next TuiState)
handleTuiEvent s e =
    case e of
        VtyEvent vtye ->
            case vtye of
                EvKey (KChar 'q') [] -> halt s
                EvKey KDown [] -> do
                    let nec = tuiStatePaths s
                    case nonEmptyCursorSelectNext nec of
                        Nothing -> continue s
                        Just nec' -> continue $ s {tuiStatePaths = nec'}
                EvKey KUp [] -> do
                    let nec = tuiStatePaths s
                    case nonEmptyCursorSelectPrev nec of
                        Nothing -> continue s
                        Just nec' -> continue $ s {tuiStatePaths = nec'}
                EvKey KEnter [] -> do
                    let fp = nonEmptyCursorCurrent $ tuiStatePaths s
                    isDirectory <- liftIO $ doesDirectoryExist fp
                    if isDirectory
                        then do
                            liftIO $ setCurrentDirectory fp
                            s' <- liftIO buildInitialState
                            continue s'
                        else continue s
                _ -> continue s
        _ -> continue s
