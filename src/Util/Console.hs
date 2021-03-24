module Util.Console where

import System.Console.ANSI
    ( setSGR,
      Color(Red),
      ColorIntensity(Vivid),
      ConsoleLayer(Foreground),
      SGR(Reset, SetColor) )

putErrorLn :: String -> IO ()
putErrorLn message =
  setSGR [SetColor Foreground Vivid Red] >> putStrLn message >> setSGR [Reset]