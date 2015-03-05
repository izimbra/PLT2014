import Control.Monad.State

import System.IO
import System.FilePath.Posix
import System.Environment
import System.Exit
import System.Process

import Fun.Abs
import Fun.Lex
import Fun.Par
import Fun.Print
import Fun.ErrM

import Interpreter

-- * Running the interpreter

-- | Lex, parse, run the interpreter with the given strategy.
--
--   In case of success, the only the computed integer should
--   be printed to the standard output.
--   Additional messages should be printed to standard error.
--
--   In case of failure, the testsuite does not impose such
--   restrictions.

run :: Strategy -> FilePath -> String -> IO ()
run strategy file s = case pProgram (myLexer s) of
  Bad err -> do
    putStrLn "SYNTAX ERROR"
    putStrLn err
    exitFailure
  Ok prg  -> case interpret strategy prg of
    Bad err -> do
      putStrLn "INTERPRETER ERROR"
      putStrLn err
      exitFailure
    Ok i -> do
      putStrLn $ show i
      hPutStrLn stderr "OK"
      exitSuccess

-- * Command line parsing

-- | The result (and state) of command line argument parsing.

data CmdLine = CmdLine
  { strategy :: Strategy
  , fileName :: FilePath
  }

-- | Default is call-by-value.

initCmdLine :: CmdLine
initCmdLine = CmdLine
  { strategy = CallByValue
  , fileName = ""
  }

-- | Print some brief help.

usage :: IO ()
usage = do
  putStrLn "Usage: lab4 [-n|-v] <SourceFile>"
  exitFailure

-- | Parse a single argument.
--
--   The evaluation strategy can be specified several times,
--   the last one counts.
--
--   Only one file name is accepted.

parseArg :: String -> StateT CmdLine IO ()
parseArg s
  | s == "-n" = modify $ \ o -> o { strategy = CallByName  }
  | s == "-v" = modify $ \ o -> o { strategy = CallByValue }
  | otherwise = do
      old <- gets fileName
      if null old then modify $ \ o -> o { fileName = s }
      else lift usage

-- * Main

-- | Parse arguments and run interpreter on file contents.

main :: IO ()
main = do
  args <- getArgs
  o <- mapM_ parseArg args `execStateT` initCmdLine
  let file = fileName o
  run (strategy o) file =<< readFile file
