{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Internal.TH.Decode
  ( withInputObject,
    withEnum,
    withInputUnion,
    decodeFieldWith,
    withScalar,
    handleEither,
  )
where

import Data.Morpheus.Internal.Utils
  ( Failure,
    failure,
    selectOr,
  )
import Data.Morpheus.Types.GQLScalar
  ( toScalar,
  )
import Data.Morpheus.Types.Internal.AST
  ( FieldName,
    InternalError,
    Message,
    ObjectEntry (..),
    ScalarValue,
    Token,
    TypeName,
    VALID,
    ValidObject,
    ValidValue,
    Value (..),
    getInputUnionValue,
    msgInternal,
  )
import Relude
  ( (.),
    Applicative (pure),
    Either (..),
    Monad ((>>=)),
    Semigroup ((<>)),
    either,
  )

withInputObject ::
  Failure InternalError m =>
  (ValidObject -> m a) ->
  ValidValue ->
  m a
withInputObject f (Object object) = f object
withInputObject _ isType = failure (typeMismatch "InputObject" isType)

-- | Useful for more restrictive instances of lists (non empty, size indexed etc)
withEnum :: Failure InternalError m => (TypeName -> m a) -> Value VALID -> m a
withEnum decode (Enum value) = decode value
withEnum _ isType = failure (typeMismatch "Enum" isType)

withInputUnion ::
  (Failure InternalError m, Monad m) =>
  (TypeName -> ValidObject -> ValidObject -> m a) ->
  ValidObject ->
  m a
withInputUnion decoder unions =
  either onFail onSucc (getInputUnionValue unions)
  where
    onSucc (name, value) = withInputObject (decoder name unions) value
    onFail = failure . msgInternal

withScalar ::
  (Applicative m, Failure InternalError m) =>
  TypeName ->
  (ScalarValue -> Either Token a) ->
  Value VALID ->
  m a
withScalar typename decodeScalar value = case toScalar value >>= decodeScalar of
  Right scalar -> pure scalar
  Left message ->
    failure
      ( typeMismatch
          ("SCALAR(" <> msgInternal typename <> ")" <> msgInternal message)
          value
      )

decodeFieldWith :: (Value VALID -> m a) -> FieldName -> ValidObject -> m a
decodeFieldWith decoder = selectOr (decoder Null) (decoder . entryValue)

handleEither :: Failure InternalError m => Either Message a -> m a
handleEither = either (failure . msgInternal) pure

-- if value is already validated but value has different type
typeMismatch :: InternalError -> Value s -> InternalError
typeMismatch text jsType =
  "Type mismatch! expected:" <> text <> ", got: "
    <> msgInternal jsType
