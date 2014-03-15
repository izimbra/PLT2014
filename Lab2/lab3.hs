import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath.Posix (takeFileName,dropExtension)
import System.Process (system)
import AbsCPP
import LexCPP
import ParCPP
import PrintCPP
import ErrM

import TypeCheckerA
import Compiler


comp :: String -> String -> IO ()
comp file s = case pProgram (myLexer s) of
              Bad err  -> do putStrLn "SYNTAX ERROR"
                             putStrLn err
                             exitFailure
              Ok  tree -> do
                case typecheck tree of
                            Bad err  -> do putStrLn "TYPE ERROR"
                                           putStrLn err
                                           exitFailure 
                            Ok tree' -> let className = dropExtension.takeFileName $ file
                                            newFile   = (dropExtension file) ++ ".j"
                                        in  do putStrLn "Type checking done"
                                               writeFile newFile $ compile className tree'
                                               putStrLn $ "Generated file " ++ newFile

main :: IO ()
main = do args <- getArgs
          case args of
            [file] -> do readFile file >>= comp file

                         system ("java -jar jasmin.jar " ++ (dropExtension file) ++ ".j")
                         return ()
            _      -> do putStrLn "Usage: lab3 <SourceFile>"
                         exitFailure


