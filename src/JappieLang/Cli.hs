{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
-- | Command line interface
module JappieLang.Cli
  ( entryPoint
  , compile
  , CompileOptions(..)
  )
where

import System.Process.Typed
import Data.String
import           Options.Applicative
import Data.Foldable
import qualified JappieLang.CodeGen as CodeGen
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text

data CompileOptions = MkCompileOptions { inputFile :: FilePath, outputFile :: FilePath }

newtype LLVMOpts = MkLLVOpts {llvmInput :: FilePath }

data CliOptions = Compile CompileOptions
                | LLVM LLVMOpts


compile :: CompileOptions -> IO ()
compile MkCompileOptions {..} = do
      -- 0. generate as code (assemble code)
      CodeGen.writeTargetAssembly inputFile "target.asm"
      -- 1. assemble
      runProcess_ $ "as -o target.o target.asm"

      -- 2. link
      -- ld doesn't work cuz we need stdlib for now
      -- TODO write a better stdlib in jappie-lang
      runProcess_ $ fromString $ "gcc target.o -o " <> outputFile


llvm :: LLVMOpts -> IO ()
llvm (MkLLVOpts input) = do
  xx <- CodeGen.genLLVMAssembly input
  Text.putStr $ Text.decodeUtf8 xx

entryPoint :: IO ()
entryPoint = do
  option <- readCliOptions
  case option of
    Compile compileOpts -> compile compileOpts
    LLVM input -> llvm input

jappieLangInputFile :: Parser FilePath
jappieLangInputFile = strOption $ long "file-in" <> metavar "FILE" <> help "input file in jappie lang"

parseCompileOptions :: Parser CompileOptions
parseCompileOptions = do
  inputFile <- jappieLangInputFile
  outputFile <- strOption $ long "file-out" <> metavar "FILE" <> help "output file, the executable" <> value "a.out"
  pure $ MkCompileOptions {..}

parseOptions :: Parser CliOptions
parseOptions = hsubparser $
  fold [
  command "llvm"
    (info (LLVM . MkLLVOpts  <$> jappieLangInputFile)
      (progDesc "emit a jappie-lang program as llvm IR"))
  ,
  command "compile"
    (info (Compile <$> parseCompileOptions)
      (progDesc "compile a jappie-lang program"))
  ]

readCliOptions :: IO CliOptions
readCliOptions = do
  customExecParser (prefs showHelpOnError) $ info
    (helper <*> parseOptions)
    (fullDesc <> Options.Applicative.header "Jappie Lang" <> progDesc
      "Cli options for the jappie language"
    )
