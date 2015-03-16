import System.Environment (getArgs)
import System.Exit (exitFailure)

import AbsFun
import LexFun
import ParFun(myLexer, pProgram)
import ErrM
import PrintFun
--import TypeChecker
import Interpreter2

-- driver
check :: Strategy -> FilePath -> IO () 
check callMode s = case pProgram (myLexer s) of
            Bad err  -> do putStrLn "SYNTAX ERROR"
                           exitFailure 
                           
            Ok  tree -> do 
--                         putStrLn $ printTree tree
--                         putStrLn $ show tree ++ "\n"
                         case (interpret callMode tree) of
                          Bad err   -> do putStrLn $ "RUNTIME ERROR: " ++ err
                                          exitFailure 
                          Ok result -> putStrLn $ show result

main :: IO ()
main = do args <- getArgs
          case args of
--            [file]      -> readFile file >>= check 
            ["-n",file] -> readFile file >>= check CallByName
            ["-v",file] -> readFile file >>= check CallByValue
            _           -> do putStrLn "Usage: lab4 [-n|-v] <SourceFile>"
                              exitFailure

