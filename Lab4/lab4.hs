import System.Environment (getArgs)
import System.Exit (exitFailure)

import AbsFP
import LexFP
import ParFP
import ErrM
import PrintFP
--import TypeChecker
import Interpreter
import InterpreterByName

-- driver

check :: Integer -> String -> IO () 
check callbyMode s = case pProgram (myLexer s) of
            Bad err  -> do putStrLn "SYNTAX ERROR"
                           putStrLn err
                           exitFailure 
                           
            Ok  tree -> do 
                        putStrLn ( show tree)
                        putStrLn "Parse OK"
                        if callbyMode == 0 
                        then interpret tree
                        else interpretByName tree
                        --case typecheck tree of
                        --  Bad err -> do putStrLn "TYPE ERROR"
                        --                putStrLn err
                        --                exitFailure 
                        --  Ok _ -> interpret tree
main :: IO ()
main = do args <- getArgs
          case args of
            [file] -> do
                    s <- readFile file
                    check 0 s --default call by value
            ["-n", file] -> do
                    s <- readFile file
                    check 1 s --call by name
            [ x  , file] -> do
                    s <- readFile file
                    check 0 s --default call by value
            _      -> do putStrLn "Usage: lab4 <SourceFile>"
                         exitFailure

