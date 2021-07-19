{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.AST.Error
  ( at,
    atPositions,
    ValidationError,
    ValidationErrors,
    toGQLError,
    Error,
    GQLError (..),
    GQLErrors,
    readErrorMessage,
    mapError,
    msgValidation,
    msgInternal,
    InternalError,
    manyMsg,
    internal,
    isInternal,
    Errors,
  )
where

import Data.Aeson
  ( FromJSON,
    Options (..),
    ToJSON (..),
    Value,
    defaultOptions,
    genericToJSON,
  )
import Data.Morpheus.Types.Internal.AST.Base
  ( Message (..),
    Msg (..),
    Position (..),
  )
import qualified Data.Text as T
import Relude

readErrorMessage :: Error -> Message
readErrorMessage = errorMessage

data Error = Error
  { errorLocations :: [Position],
    isInternal :: Bool,
    errorMessage :: Message
  }
  deriving (Show)

instance IsString Error where
  fromString = Error [] False . msg

instance Semigroup Error where
  Error m1 p1 x1 <> Error m2 p2 x2 = Error (m1 <> m2) (p1 || p2) (x1 <> x2)

internal :: Error -> Error
internal x = x {isInternal = True}

at :: Error -> Position -> Error
at err pos = atPositions err [pos]
{-# INLINE at #-}

atPositions :: Foldable t => Error -> t Position -> Error
atPositions (Error ps x m) pos = Error (ps <> toList pos) x m
{-# INLINE atPositions #-}

{-# DEPRECATED ValidationError "use Error" #-}

type ValidationError = Error

{-# DEPRECATED ValidationErrors "use Errors" #-}

type ValidationErrors = [ValidationError]

type Errors = [Error]

toGQLError :: Error -> GQLError
toGQLError (Error p x m) = GQLError (if x then "Internal Error! " <> m else m) p Nothing
{-# INLINE toGQLError #-}

msgValidation :: (Msg a) => a -> ValidationError
msgValidation = Error [] False . msg
{-# INLINE msgValidation #-}

manyMsg :: (Foldable t, Msg a) => t a -> Error
manyMsg = Error [] False . Message . T.intercalate ", " . fmap (readMessage . msg) . toList

mapError :: (Message -> Message) -> ValidationError -> GQLError
mapError f (Error locations _ text) =
  GQLError
    { message = f text,
      locations,
      extensions = Nothing
    }

type InternalError = Error

msgInternal :: (Msg a) => a -> InternalError
msgInternal = Error [] True . msg
{-# INLINE msgInternal #-}

instance Msg Error where
  msg = errorMessage

data GQLError = GQLError
  { message :: Message,
    locations :: [Position],
    extensions :: Maybe Value
  }
  deriving
    ( Show,
      Eq,
      Generic,
      FromJSON
    )

instance ToJSON GQLError where
  toJSON = genericToJSON (defaultOptions {omitNothingFields = True})

type GQLErrors = [GQLError]
