{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Parsing.Internal.Internal
  ( Parser,
    Position,
    getLocation,
    processParser,
  )
where

import Data.ByteString.Lazy (ByteString)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Morpheus.Ext.Result
  ( Result (..),
    ValidationResult,
  )
import Data.Morpheus.Types.Internal.AST
  ( Position (..),
    ValidationError,
    at,
    msgValidation,
  )
import Relude hiding (ByteString)
import Text.Megaparsec
  ( ParseError,
    ParseErrorBundle
      ( ParseErrorBundle
      ),
    ParsecT,
    SourcePos,
    SourcePos (..),
    attachSourcePos,
    bundleErrors,
    bundlePosState,
    errorOffset,
    getSourcePos,
    parseErrorPretty,
    runParserT,
    unPos,
  )

getLocation :: Parser Position
getLocation = fmap toLocation getSourcePos
{-# INLINEABLE getLocation #-}

toLocation :: SourcePos -> Position
toLocation SourcePos {sourceLine, sourceColumn} =
  Position {line = unPos sourceLine, column = unPos sourceColumn}
{-# INLINEABLE toLocation #-}

type MyError = Void

type Parser = ParsecT MyError ByteString ValidationResult

type ErrorBundle = ParseErrorBundle ByteString MyError

processParser :: Parser a -> ByteString -> ValidationResult a
processParser parser txt = case runParserT parser [] txt of
  Success {result} -> case result of
    Right root -> pure root
    Left parseError -> Failure (parseErrorToGQLError <$> bundleToErrors parseError)
  Failure {errors} -> Failure errors

parseErrorToGQLError :: (ParseError ByteString MyError, SourcePos) -> ValidationError
parseErrorToGQLError (err, position) = msgValidation (parseErrorPretty err) `at` toLocation position

bundleToErrors ::
  ErrorBundle -> [(ParseError ByteString MyError, SourcePos)]
bundleToErrors ParseErrorBundle {bundleErrors, bundlePosState} =
  NonEmpty.toList $ fst $
    attachSourcePos
      errorOffset
      bundleErrors
      bundlePosState
