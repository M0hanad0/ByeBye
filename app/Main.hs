{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Foldable (traverse_)
import Data.GI.Base
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.Gtk as Gtk
import System.Directory
import System.Process

data ImageButton = ImageButton
  { imgBtnImage :: Gtk.Image,
    imgBtnButton :: Gtk.Button,
    imgBtnLabel :: Gtk.Label,
    imgBtnAction :: IO ()
  }

imageButton :: Text -> Text -> IO () -> IO ImageButton
imageButton imgPath labelMarkup action = do
  img <- Gtk.imageNewFromFile $ T.unpack imgPath
  label <- Gtk.labelNew Nothing
  Gtk.labelSetMarkup label labelMarkup
  btn <- Gtk.buttonNew
  Gtk.buttonSetRelief btn Gtk.ReliefStyleNone
  Gtk.buttonSetImage btn $ Just img
  Gtk.widgetSetHexpand btn False
  on btn #clicked action
  return $ ImageButton {imgBtnButton = btn, imgBtnImage = img, imgBtnLabel = label, imgBtnAction = action}

surround :: Text -> Text -> Text -> Text
surround prefix suffix string = prefix `T.append` string `T.append` suffix

surroundTag :: Text -> Text -> Text
surroundTag = (surround . surround "<" ">") <*> (surround "</" ">")

setup :: Gtk.Window -> IO ()
setup win = do
  Gtk.setContainerBorderWidth win 10
  Gtk.setWindowTitle win "ByeBye"
  Gtk.setWindowResizable win False
  Gtk.setWindowDefaultWidth win 750
  Gtk.setWindowDefaultHeight win 225
  Gtk.setWindowWindowPosition win Gtk.WindowPositionCenter
  Gtk.windowSetDecorated win False

main :: IO ()
main =
  do
    Gtk.init Nothing

    win <- Gtk.windowNew Gtk.WindowTypeToplevel
    setup win

    home <- T.pack <$> getHomeDirectory
    pwd <- T.pack <$> getCurrentDirectory

    let commands =
          [ ("cancel", Gtk.widgetDestroy win),
            ("logout", callCommand "dm-tool switch-to-greeter"),
            ("reboot", callCommand "reboot"),
            ("shutdown", callCommand "shutdown now"),
            ("suspend", callCommand "systemctl suspend"),
            ("hibernate", callCommand "systemctl hibernate"),
            ("lock", callCommand "slock")
          ]

    btns <-
      traverse
        ( \(cmdName, cmd) ->
            imageButton
              (pwd `T.append` "/img/" `T.append` cmdName `T.append` ".png")
              (surroundTag "b" (T.toTitle cmdName))
              cmd
        )
        commands

    grid <- Gtk.gridNew
    Gtk.gridSetColumnSpacing grid 10
    Gtk.gridSetRowSpacing grid 10
    Gtk.gridSetColumnHomogeneous grid True

    traverse_
      ( \(i, imgBtn) -> do
          #attach grid (imgBtnButton imgBtn) i 0 1 1
          #attach grid (imgBtnLabel imgBtn) i 1 1 1
      )
      $ zip [0 ..] btns

    #add win grid

    Gtk.onWidgetDestroy win Gtk.mainQuit
    #showAll win
    Gtk.main
