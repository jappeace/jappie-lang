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
import qualified Data.Text as Text
import qualified Data.ByteString as BS
import System.Directory(createDirectoryIfMissing)
import System.FilePath

data CompileOptions = MkCompileOptions
  { inputFile :: FilePath
  , outputFile :: FilePath
  , workDir :: FilePath
  }

defaultCompileOptions :: CompileOptions
defaultCompileOptions = MkCompileOptions
  { inputFile = "in.jappie"
  , outputFile = "a.out"
  , workDir = "build"
  }


newtype LLVMOpts = MkLLVOpts {llvmInput :: FilePath }

data CliOptions = Compile CompileOptions
                | LLVM LLVMOpts

compile :: CompileOptions -> IO ()
compile MkCompileOptions {..} = do
      -- 0. generate as code (assemble code)
      assamblyBs <- CodeGen.writeTargetAssembly inputFile

      let asText = Text.decodeUtf8 assamblyBs
      createDirectoryIfMissing True workDir
      let target = workDir </> "target.asm"
      BS.writeFile target assamblyBs
      -- 1. assemble
      runProcess_ $ fromString $ "as -o target.o " <> target

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
  outputFile <- strOption $ long "file-out" <> metavar "FILE" <> help "output file, the executable" <> value (outputFile defaultCompileOptions)
  workDir <- strOption $ long "work-dir" <> metavar "PATH" <> help "work dir for intermediate files" <> value (workDir defaultCompileOptions)
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
