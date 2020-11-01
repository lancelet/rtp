{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception.Safe (bracket)
import Control.Monad (forM_, unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Foreign (Ptr, alloca, malloc, mallocArray, nullPtr, peek, withArray)
import Foreign.C.String (peekCString)
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
  _program <- makeVertexFragmentProgram "shaders/shader.vert" "shaders/shader.frag"
  appLoop window
  SDL.destroyWindow window
  SDL.quit

highDPIOpenGLWindow :: SDL.WindowConfig
highDPIOpenGLWindow =
  SDL.defaultWindow
    { SDL.windowInitialSize = V2 screenWidth screenHeight,
      SDL.windowHighDPI = True,
      SDL.windowGraphicsContext =
        SDL.OpenGLContext
          ( SDL.defaultOpenGL
              { SDL.glProfile = SDL.Core SDL.Debug 3 3
              }
          )
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

---- SHADER STUFF

newtype ShaderProgram = ShaderProgram {unShaderProgram :: GL.GLuint}

newtype Shader = Shader {unShader :: GL.GLuint}

data ShaderType
  = VertexShader
  | FragmentShader

makeVertexFragmentProgram :: FilePath -> FilePath -> IO ShaderProgram
makeVertexFragmentProgram vertexFile fragmentFile =
  bracket
    (makeShaderFromFile VertexShader vertexFile)
    deleteShader
    $ \vertexShader ->
      bracket
        (makeShaderFromFile FragmentShader fragmentFile)
        deleteShader
        $ \fragmentShader ->
          makeProgram [vertexShader, fragmentShader]

makeProgram :: [Shader] -> IO ShaderProgram
makeProgram shaders = do
  -- create the program; it's identified by a uint
  uintId <- GL.glCreateProgram

  -- attach all the shaders to the program
  forM_ shaders $ \shader ->
    GL.glAttachShader uintId (unShader shader)

  -- link the program
  GL.glLinkProgram uintId

  -- check if the link was successful
  linkedOk <- do
    alloca $ \programOk -> do
      GL.glGetProgramiv uintId GL.GL_LINK_STATUS programOk
      programOkVal <- peek programOk
      pure $ programOkVal == GL.GL_TRUE

  -- if the linking failed; error with some useful information
  unless linkedOk $ do
    -- fetch the length of the log message
    logLengthPtr <- malloc :: IO (Ptr GL.GLint)
    GL.glGetProgramiv uintId GL.GL_INFO_LOG_LENGTH logLengthPtr
    logLength <- peek logLengthPtr

    -- fetch the log message
    logMessagePtr <- mallocArray (fromIntegral logLength) :: IO (Ptr GL.GLchar)
    GL.glGetProgramInfoLog uintId logLength nullPtr logMessagePtr
    logMessage <- peekCString logMessagePtr

    -- raise an exception with an error
    error $ "Shader program failed to link: " <> logMessage

  pure (ShaderProgram uintId)

shaderTypeToGLenum :: ShaderType -> GL.GLenum
shaderTypeToGLenum shaderType = case shaderType of
  VertexShader -> GL.GL_VERTEX_SHADER
  FragmentShader -> GL.GL_FRAGMENT_SHADER

makeShaderFromFile :: ShaderType -> FilePath -> IO Shader
makeShaderFromFile shaderType fileName = do
  bsShaderSource <- B.readFile fileName
  makeShader shaderType bsShaderSource

makeShader :: ShaderType -> ByteString -> IO Shader
makeShader shaderType source = do
  -- create the shader; it's identified by a uint
  uintId <- GL.glCreateShader . shaderTypeToGLenum $ shaderType

  -- set the source for the shader
  -- NB: glShaderSource copies the string containing the shader source, so we
  --     can free our copies of the pointers immediately afterward
  B.useAsCString source $ \cString ->
    withArray [cString] $ \strPtr ->
      withArray [fromIntegral (B.length source)] $ \lenPtr ->
        GL.glShaderSource uintId 1 strPtr lenPtr

  -- compile the shader
  GL.glCompileShader uintId

  -- check if the shader compiled OK
  compiledOk <- do
    alloca $ \shaderOk -> do
      GL.glGetShaderiv uintId GL.GL_COMPILE_STATUS shaderOk
      shaderOkVal <- peek shaderOk
      pure $ shaderOkVal == GL.GL_TRUE

  -- if the shader failed to compile; dump some useful logging info
  unless compiledOk $ do
    -- fetch the length of the log message
    logLengthPtr <- malloc :: IO (Ptr GL.GLint)
    GL.glGetShaderiv uintId GL.GL_INFO_LOG_LENGTH logLengthPtr
    logLength <- peek logLengthPtr

    -- get the log message
    logMessagePtr <- mallocArray (fromIntegral logLength) :: IO (Ptr GL.GLchar)
    GL.glGetShaderInfoLog uintId logLength nullPtr logMessagePtr
    logMessage <- peekCString logMessagePtr

    -- raise an exception with the error
    error $ "Shader failed to compile: " <> logMessage

  -- return the shader
  pure (Shader uintId)

deleteShader :: Shader -> IO ()
deleteShader = GL.glDeleteShader . unShader
