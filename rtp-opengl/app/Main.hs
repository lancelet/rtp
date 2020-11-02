{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception.Safe (bracket)
import Control.Monad (forM_, unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Foreign (Ptr, alloca, castPtr, malloc, mallocArray, nullPtr, peek, sizeOf, withArray)
import Foreign.C.String (peekCString)
import qualified Graphics.GL as GL
import qualified SDL
import SDL.Vect (V2 (V2))

screenWidth, screenHeight :: Int
screenWidth = 800
screenHeight = 600

data AppState = AppState
  { shaderProgram :: ShaderProgram,
    vertexBuffer :: Buffer
  }

main :: IO ()
main = do
  SDL.initializeAll
  window <- SDL.createWindow "RTP - SDL Window" highDPIOpenGLWindow
  _ <- SDL.glCreateContext window
  shaderProg <- makeVertexFragmentProgram "shaders/shader.vert" "shaders/shader.frag"
  vertexBuf <- createTriangleVertexBuffer
  vaoUint <- alloca $ \ptr -> do
    GL.glGenVertexArrays 1 ptr
    peek ptr
  GL.glBindVertexArray vaoUint
  let appState =
        AppState
          { shaderProgram = shaderProg,
            vertexBuffer = vertexBuf
          }
  appLoop window appState
  SDL.destroyWindow window
  SDL.quit

highDPIOpenGLWindow :: SDL.WindowConfig
highDPIOpenGLWindow =
  SDL.defaultWindow
    { SDL.windowInitialSize =
        V2
          (fromIntegral screenWidth)
          (fromIntegral screenHeight),
      SDL.windowHighDPI = True,
      SDL.windowGraphicsContext =
        SDL.OpenGLContext
          ( SDL.defaultOpenGL
              { SDL.glProfile = SDL.Core SDL.Debug 3 3
              }
          )
    }

appLoop :: SDL.Window -> AppState -> IO ()
appLoop window appState = do
  V2 w h <- SDL.glGetDrawableSize window
  GL.glViewport 0 0 (fromIntegral w) (fromIntegral h)
  GL.glClear GL.GL_COLOR_BUFFER_BIT
  draw appState
  SDL.glSwapWindow window

  events <- SDL.pollEvents
  let qPressed = any eventIsQPress events
  unless qPressed (appLoop window appState)

draw :: AppState -> IO ()
draw appState = do
  bindBuffer GL.GL_ARRAY_BUFFER (vertexBuffer appState)
  GL.glVertexAttribPointer 0 3 GL.GL_FLOAT GL.GL_FALSE (fromIntegral (3 * sizeOf (undefined :: GL.GLfloat))) nullPtr
  GL.glEnableVertexAttribArray 0

  useProgram (shaderProgram appState)

  GL.glDrawArrays GL.GL_TRIANGLES 0 3
  GL.glDisableVertexAttribArray 0

eventIsQPress :: SDL.Event -> Bool
eventIsQPress event =
  case SDL.eventPayload event of
    SDL.KeyboardEvent keyboardEvent ->
      SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed
        && SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeQ
    _ -> False

---- BUFFERS

newtype Buffer = Buffer {unBuffer :: GL.GLuint}

createTriangleVertexBuffer :: IO Buffer
createTriangleVertexBuffer = do
  let vertexData = [-1, -1, 0, 1, -1, 0, 0, 1, 0] :: [GL.GLfloat]
  alloca $ \ptr -> do
    GL.glGenBuffers 1 ptr
    vboId <- peek ptr
    GL.glBindBuffer GL.GL_ARRAY_BUFFER vboId
    withArray vertexData $ \array -> do
      let len = fromIntegral (sizeOf (undefined :: GL.GLfloat) * length vertexData)
      GL.glBufferData GL.GL_ARRAY_BUFFER len (castPtr array) GL.GL_STATIC_DRAW
    pure (Buffer vboId)

bindBuffer :: GL.GLenum -> Buffer -> IO ()
bindBuffer target = GL.glBindBuffer target . unBuffer

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
        $ \fragmentShader -> do
          program <- makeProgram [vertexShader, fragmentShader]
          detachShader program vertexShader
          detachShader program fragmentShader
          pure program

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

detachShader :: ShaderProgram -> Shader -> IO ()
detachShader program shader =
  GL.glDetachShader (unShaderProgram program) (unShader shader)

useProgram :: ShaderProgram -> IO ()
useProgram = GL.glUseProgram . unShaderProgram
