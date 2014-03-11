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

check :: Integer -> String -> IO () 
check i s = case pProgram (myLexer s) of
            Bad err  -> do putStrLn "SYNTAX ERROR"
                           putStrLn err
                           exitFailure 
                           
            Ok  tree -> do 
                        putStrLn ( show tree)
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
            [file] -> do
                    s <- readFile file
                    check 0 s
            ["-n", file] -> do
                    s <- readFile file
                    check 1 s
            [ x  , file] -> do
                    s <- readFile file
                    check 0 s
            _      -> do putStrLn "Usage: lab4 <SourceFile>"
                         exitFailure

