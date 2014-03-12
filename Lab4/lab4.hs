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

check :: String -> IO () 
check s = case pProgram (myLexer s) of
            Bad err  -> do putStrLn "SYNTAX ERROR"
                           putStrLn err
                           exitFailure 
                           
            Ok  tree -> do 
                        --putStrLn ( show tree)
                        putStrLn $ printTree tree
                        putStrLn "Parse OK"
                        interpret tree
                        --case typecheck tree of
                        --  Bad err -> do putStrLn "TYPE ERROR"
                        --                putStrLn err
                        --                exitFailure 
                        --  Ok _ -> interpret tree

main :: IO ()
main = do args <- getArgs
          case args of
            [file] -> readFile file >>= check
            _      -> do putStrLn "Usage: lab4 <SourceFile>"
                         exitFailure

