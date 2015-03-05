import System.Environment (getArgs)
import System.Exit (exitFailure)

import AbsFP
import LexFP
import ParFP
import ErrM
import PrintFP
--import TypeChecker
import Interpreter

-- driver
check :: Bool -> String -> IO () 
check callMode s = case pProgram (myLexer s) of
            Bad err  -> do putStrLn "SYNTAX ERROR"
                           putStrLn err
                           exitFailure 
                           
            Ok  tree -> do putStrLn $ printTree tree
                           interpret tree callMode

main :: IO ()
main = do args <- getArgs
          case args of
            [file]      -> readFile file >>= check False 
            ["-n",file] -> readFile file >>= check True
            ["-v",file] -> readFile file >>= check False 
            _           -> do putStrLn "Usage: lab4 [-n|-v] <SourceFile>"
                              exitFailure

