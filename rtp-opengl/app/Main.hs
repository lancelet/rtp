{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (unless)
import Foreign.C.Types (CInt)
import qualified Graphics.GL as GL
import qualified SDL
import SDL.Vect (V2 (V2))

screenWidth, screenHeight :: CInt
screenWidth = 800
screenHeight = 600

main :: IO ()
main = do
  SDL.initializeAll
  window <- SDL.createWindow "RTP - SDL Window" highDPIOpenGLWindow
  _ <- SDL.glCreateContext window
  appLoop window

highDPIOpenGLWindow :: SDL.WindowConfig
highDPIOpenGLWindow =
  SDL.defaultWindow
    { SDL.windowInitialSize = V2 screenWidth screenHeight,
      SDL.windowHighDPI = True,
      SDL.windowGraphicsContext = SDL.OpenGLContext SDL.defaultOpenGL
    }

appLoop :: SDL.Window -> IO ()
appLoop window = do
  events <- SDL.pollEvents
  let qPressed = any eventIsQPress events
  GL.glClear GL.GL_COLOR_BUFFER_BIT
  draw
  SDL.glSwapWindow window
  unless qPressed (appLoop window)

draw :: IO ()
draw = do
  GL.glClear GL.GL_COLOR_BUFFER_BIT

eventIsQPress :: SDL.Event -> Bool
eventIsQPress event =
  case SDL.eventPayload event of
    SDL.KeyboardEvent keyboardEvent ->
      SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed
        && SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeQ
    _ -> False
