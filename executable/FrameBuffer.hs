module Main where

import Control.Monad.Loops (whileM_)
import Data.Functor ((<$>))
import qualified Data.Vector.Storable as V
import Data.Maybe (fromJust)
import Data.Default
import Data.Word

import qualified Graphics.UI.GLFW as GLFW
import Linear
import Graphics.GL.Low

import Paths_lowgl_examples

main = do
  GLFW.init
  GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
  GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 2)
  GLFW.windowHint (GLFW.WindowHint'OpenGLForwardCompat True)
  GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)
  GLFW.windowHint (GLFW.WindowHint'Samples 4) -- CM
  mwin <- GLFW.createWindow 640 480 "Framebuffer" Nothing Nothing
  case mwin of
    Nothing  -> putStrLn "createWindow failed"
    Just win -> do
      GLFW.makeContextCurrent (Just win)

      -- CM
      (w,h) <- GLFW.getFramebufferSize win
      setViewport $ Viewport 0 0 w h
      
      GLFW.swapInterval 1
      (vao1, vao2, prog1, prog2, fbo, texture) <- setup
      whileM_ (not <$> GLFW.windowShouldClose win) $ do
        GLFW.pollEvents
        t <- (realToFrac . fromJust) <$> GLFW.getTime
        draw vao1 vao2 prog1 prog2 fbo texture t
        GLFW.swapBuffers win

setup = do
  -- primary subject
  vao1 <- newVAO
  bindVAO vao1
  let blob = V.fromList
        [ -0.5, -0.5, 0, 0
        ,  0,    0.5, 0, 1
        ,  0.5, -0.5, 1, 1] :: V.Vector Float
  vbo <- newVBO blob StaticDraw
  bindVBO vbo
  vsource  <- readFile =<< getDataFileName "framebuffer.vert"
  fsource1 <- readFile =<< getDataFileName "framebuffer1.frag"
  prog1 <- newProgram vsource fsource1
  useProgram prog1
  setVertexLayout
    [ Attrib "position" 2 GLFloat
    , Attrib "texcoord" 2 GLFloat ]

  -- full-screen quad to show the post-processed scene
  vao2 <- newVAO
  bindVAO vao2
  let blob = V.fromList
        [ -1, -1, 0, 0
        , -1,  1, 0, 1
        ,  1, -1, 1, 0
        ,  1,  1, 1, 1] :: V.Vector Float
  vbo <- newVBO blob StaticDraw
  bindVBO vbo
  indices <- newElementArray (V.fromList [0,1,2,3,2,1] :: V.Vector Word8) StaticDraw
  bindElementArray indices
  fsource2 <- readFile =<< getDataFileName "framebuffer2.frag"
  prog2 <- newProgram vsource fsource2
  useProgram prog2
  setVertexLayout
    [ Attrib "position" 2 GLFloat
    , Attrib "texcoord" 2 GLFloat ]

  -- create an FBO to render the primary scene on
  fbo <- newFBO
  bindFramebuffer fbo
  texture <- newEmptyTexture2D 640 480 :: IO (Tex2D RGB)
  bindTexture2D texture
  setTex2DFiltering Linear
  attachTex2D texture
  return (vao1, vao2, prog1, prog2, fbo, texture)

draw :: VAO -> VAO -> Program -> Program -> FBO -> Tex2D RGB -> Float -> IO ()
draw vao1 vao2 prog1 prog2 fbo texture t = do
  -- render primary scene to fbo
  bindVAO vao1
  bindFramebuffer fbo
  useProgram prog1
  clearColorBuffer (0,0,0)
  setUniform1f "time" [t]
  drawTriangles 3

  -- render results to quad on main screen
  bindVAO vao2
  bindFramebuffer DefaultFramebuffer
  useProgram prog2
  bindTexture2D texture
  clearColorBuffer (0,0,0)
  setUniform1f "time" [t]
  drawIndexedTriangles 6 UByteIndices