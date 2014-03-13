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

-- driver

-- check :: String -> IO () 
-- check s = case pProgram (myLexer s) of
--             Bad err  -> do putStrLn "SYNTAX ERROR"
--                            putStrLn err
--                            exitFailure 
--             Ok  tree -> case typecheck tree of
--                           Bad err -> do putStrLn "TYPE ERROR"
--                                         putStrLn err
--                                         exitFailure 
--                           --Ok _ -> putStrLn "Ok" --
--                           Ok _ -> compile s tree -- for testing w/o interpreter

comp :: String -> String -> IO ()
comp file s = case pProgram (myLexer s) of
              Bad err  -> do putStrLn "SYNTAX ERROR"
                             putStrLn err
                             exitFailure
              Ok  tree -> do
                putStrLn (printTree tree)
                case typecheck tree of
                            Bad err  -> do putStrLn "TYPE ERROR"
                                           putStrLn err
                                           exitFailure 
                            Ok tree' -> let className = dropExtension.takeFileName $ file
                                            newFile   = (dropExtension file) ++ ".j"
                                           -- use tree' for annotated type checker
                                        in  do putStrLn "Type checking done"
--                                               putStrLn $ show tree
                                               putStrLn $ "\n\n---\n\n"
                                               putStrLn $ show tree'
                                               putStrLn $ "\nEND OF TREE\n"
                                               writeFile newFile $ compile className tree'
                                               putStrLn $ "Generated file " ++ newFile

main :: IO ()
main = do args <- getArgs
          case args of
            [file] -> do readFile file >>= comp file
                         -- source   <- readFile file
--                         let fileName = dropExtension.takeFileName $ file 
                         -- comp fileName source
                         system ("java -jar jasmin.jar " ++ (dropExtension file) ++ ".j")
                         return ()
            _      -> do putStrLn "Usage: lab3 <SourceFile>"
                         exitFailure

-- main :: IO ()
-- main = do args <- getArgs
--           case args of
--             [file] -> do
--                                                    s <- readFile file
--                                                                   comp "Foo" s               --- name Foo instead of (takeWhile (/='.') file)
--                system "java -jar jasmin.jar Foo.j"
--                                    return ()
--                                                _      -> do putStrLn "Usage: lab3 <SourceFile>"
--                          exitFailure

