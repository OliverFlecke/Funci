module Runner where

import System.IO
import Control.Monad
import Evaluator

getProgram :: String -> IO String
getProgram file = readFile file