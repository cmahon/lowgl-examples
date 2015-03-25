module Main where

import Control.Monad.Loops (whileM_)
import Data.Functor ((<$>))
import qualified Data.Vector.Storable as V
import Codec.Picture
import Data.Either.Combinators (fromRight')
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
  mwin <- GLFW.createWindow 640 480 "Texture" Nothing Nothing
  case mwin of
    Nothing  -> putStrLn "createWindow failed"
    Just win -> do
      GLFW.makeContextCurrent (Just win)
      
      -- CM
      (w,h) <- GLFW.getFramebufferSize win
      setViewport $ Viewport 0 0 w h
      
      GLFW.swapInterval 1
      (vao, prog, texture) <- setup
      whileM_ (not <$> GLFW.windowShouldClose win) $ do
        GLFW.pollEvents
        draw vao prog texture
        GLFW.swapBuffers win

setup = do
  -- establish a VAO
  vao <- newVAO
  bindVAO vao
  -- load the shader
  vsource <- readFile =<< getDataFileName "texture.vert"
  fsource <- readFile =<< getDataFileName "texture.frag"
  prog <- newProgram vsource fsource
  useProgram prog
  -- load the vertices
  let blob = V.fromList -- a quad has four vertices
        [ -0.5, -0.5, 0, 1
        , -0.5,  0.5, 0, 0
        ,  0.5, -0.5, 1, 1
        ,  0.5,  0.5, 1, 0 ] :: V.Vector Float
  vbo <- newVBO blob StaticDraw
  bindVBO vbo
  setVertexLayout [ Attrib "position" 2 GLFloat
                  , Attrib "texcoord" 2 GLFloat ]
  -- load the element array to draw a quad with two triangles
  indices <- newElementArray (V.fromList [0,1,2,3,2,1] :: V.Vector Word8) StaticDraw
  bindElementArray indices
  -- load the texture with JuicyPixels
  -- let fromRight (Left x) = error x
  --     fromRight (Right x) = x
  ImageRGBA8 (Image w h image) <- ((return . fromRight') =<< readImage =<< getDataFileName "logo.png")
  texture <- newTexture2D image (Dimensions w h) :: IO (Tex2D RGBA)
  setTex2DFiltering Linear
  return (vao, prog, texture)

draw vao prog texture = do
  clearColorBuffer (0.5, 0.5, 0.5)
  bindVAO vao
  useProgram prog
  bindTexture2D texture
  drawIndexedTriangles 6 UByteIndices