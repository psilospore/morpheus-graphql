{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Parsing.Internal.Internal
  ( Parser,
    Position,
    getLocation,
    processParser,
  )
where

import Control.Monad.Except (MonadError (throwError))
import Data.ByteString.Lazy (ByteString)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Morpheus.Ext.Result
  ( Eventless,
    Eventless2,
    Result (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( GQLError (..),
    Msg (msg),
    Position (..),
    toGQLError,
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

type Parser = ParsecT MyError ByteString Eventless2

type ErrorBundle = ParseErrorBundle ByteString MyError

processParser :: Parser a -> ByteString -> Eventless a
processParser parser txt = case runParserT parser [] txt of
  Success {result} -> case result of
    Right root -> pure root
    Left parseError -> throwError (parseErrorToGQLError <$> bundleToErrors parseError)
  Failure {errors} -> throwError (map toGQLError errors)

parseErrorToGQLError :: (ParseError ByteString MyError, SourcePos) -> GQLError
parseErrorToGQLError (err, position) =
  GQLError
    { message = msg (parseErrorPretty err),
      locations = [toLocation position],
      extensions = Nothing
    }

bundleToErrors ::
  ErrorBundle -> [(ParseError ByteString MyError, SourcePos)]
bundleToErrors ParseErrorBundle {bundleErrors, bundlePosState} =
  NonEmpty.toList $ fst $
    attachSourcePos
      errorOffset
      bundleErrors
      bundlePosState
