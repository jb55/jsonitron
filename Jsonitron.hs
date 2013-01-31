{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import           Options.Applicative
import           Blaze.ByteString.Builder (fromByteString, toByteString)
import           Control.Applicative
import           Data.Monoid
import           Control.Arrow ((>>>))
import           Control.Monad.Trans
import           Data.Aeson
import           Data.ByteString (ByteString)
import           Data.Conduit (($$), (<+<), (>+>))
import           Data.Maybe (fromMaybe)
import           System.IO (stdin, stdout)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.HashMap.Strict as M

io = liftIO

stdinSource :: MonadIO m => C.GSource m ByteString
stdinSource = CB.sourceHandle stdin

stdoutSink :: MonadIO m => C.GInfSink ByteString m
stdoutSink = CB.sinkHandle stdout

stripNulls :: Value -> Value
stripNulls (Array a) = Array $ fmap stripNulls a
stripNulls (Object o) = Object $ M.map stripNulls $ M.filter (/= Null) o
stripNulls v = v

unlines' :: BL.ByteString -> BL.ByteString
unlines' = flip BL.append "\n"

data Options = Options { optLined :: Bool }


optsParser :: Parser Options
optsParser = Options
  <$> switch
      ( long "lined"
     <> short 'l'
     <> help "Treat as individual JSON objects per line" )

use o = info (helper <*> o)
             (fullDesc <> progDesc "jsonitron"
                       <> header "jsonitron - json manipulator")

processStreaming opts src dest =
  C.runResourceT $ src >+> CB.lines >+> process opts $$ dest
processConsumed opts src dest = do
  x <- C.runResourceT $ src >+> CL.map fromByteString $$ CL.consume
  let d = toByteString . mconcat $ x
  C.runResourceT $ (C.yield d) >+> process opts $$ dest

run :: Options -> (Value -> Value) -> ByteString -> ByteString
run opts f =
  let lined = optLined opts
  in BL.fromStrict
    >>> decode
    >>> fromMaybe (error "ERROR: failed to decode")
    >>> f
    >>> encode
    >>> (if lined then unlines' else id)
    >>> BL.toStrict

process :: Monad m => Options -> C.GInfConduit ByteString m ByteString
process opts = CL.map (run opts stripNulls)

main = do
  opts <- execParser (use optsParser)
  let src = stdinSource
      dst = stdoutSink
      run' fn = fn opts src dst
  if optLined opts
    then run' processStreaming
    else run' processConsumed
