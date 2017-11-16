module Util where

import System.Exit (exitFailure)

printFail :: Show a => a -> IO b
printFail msg = print msg >> exitFailure
