{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Graphics.UI.FLTK.LowLevel.FL as FL
import Graphics.UI.FLTK.LowLevel.Fl_Types
import Graphics.UI.FLTK.LowLevel.Fl_Enumerations
import Graphics.UI.FLTK.LowLevel.FLTKHS
import Data.Text
import Data.IORef

buttonCb :: Ref Button -> IO ()
buttonCb b' = do
  putStrLn "buttonCb called"
  l' <- getLabel b'
  if (l' == "Hello world")
    then setLabel b' "Goodbye world"
    else setLabel b' "Hello world"

drawHM :: Ref Widget -> IO ()
drawHM widget = do
  rect' <- getRectangle widget
  flcPushClip rect'
  flcRectWithColor rect' darkBlueColor

--handleHM :: Ref Widget -> Event -> IO (Either UnknownEvent (), ByteString)
handleHM :: (IORef (Int)) -> Ref Widget -> Event -> IO (Either UnknownEvent ())
handleHM cnt widget ev = 
  case ev of
    e' | e' == Push || e' == Drag -> do
      modifyIORef' cnt (+1)
      putStr "Handling: "
      putStrLn $ show ev
      x <- FL.eventX
      y <- FL.eventY
      --let t' = 
      putStr (show (ByX(fromIntegral x)))
      putStr " "
      putStr (show (ByY(fromIntegral y)))
      putStr " Count: "
      c <- readIORef cnt
      putStrLn ( show c)
      --putStr (

      flcSetColor whiteColor
      flcCircle (ByXY (ByX $ fromIntegral x) (ByY $ fromIntegral y)) 4.0
      -- do stuff
      return (Right ())
    _ -> do return (Left UnknownEvent)

ui :: IO ()
ui = do
  count <- newIORef (0 :: Int)
  window <- windowNew
    (Size (Width 1000) (Height 700))
    Nothing
    Nothing
  FL.getSystemColors
  begin window
  b' <- buttonNew
    (Rectangle (Position (X 100) (Y 550)) (Size (Width 95) (Height 30)))
    (Just "Hello world")
  
  setLabelsize b' (FontSize 10)
  setCallback b' buttonCb
  hm' <- widgetCustom
    (Rectangle (Position (X 5) (Y 5)) (Size (Width 900) (Height 500)))
    (Just "H")
    (drawHM)
    (defaultCustomWidgetFuncs {
      handleCustom = (Just (handleHM count))
    })
    --defaultCustomWidgetFuncs
  setColor hm' (Color 248)
  end window
  showWidget window

main :: IO ()
main = ui >> FL.run >> FL.flush

replMain :: IO ()
replMain = ui >> FL.replRun

