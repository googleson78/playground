module MinimalThreepenny where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI


main' :: IO ()
main' = do
  startGUI defaultConfig setup

setup :: Window -> UI ()
setup rootWindow = do
  userNameInput <- UI.input # set (attr "placeholder") "User name"
  loginButton <- UI.button #+ [ string "Login" ]
  getBody rootWindow #+
    map element [ userNameInput, loginButton ]

  on UI.click loginButton $ \_ -> do
    userName <- get value userNameInput

    let currentItems = []

    let showItem item = UI.li #+ [ string item ]
    toDoContainer <- UI.ul #+ map showItem currentItems

    newItem <- UI.input

    on UI.sendValue newItem $ \input -> do
      set UI.value "" (element newItem)
      element toDoContainer #+ [ showItem input ]

    header <- UI.h1 #+ [ string $ userName ++ "'s To-Do List" ]
    set children
      [ header, toDoContainer, newItem ]
      (getBody rootWindow)
