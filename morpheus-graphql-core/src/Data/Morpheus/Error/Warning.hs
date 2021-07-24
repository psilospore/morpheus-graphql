{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Error.Warning
  ( renderGQLErrors,
    deprecatedEnum,
    deprecatedField,
    gqlWarnings,
  )
where

import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Morpheus.Types.Internal.AST.Base
  ( Description,
    Ref (..),
  )
import Data.Morpheus.Types.Internal.AST.Error
  ( GQLError,
    GQLErrors,
    ValidationError,
    at,
    msg,
    msgValidation,
  )
import Data.Morpheus.Types.Internal.AST.Name
  ( FieldName,
  )
import Language.Haskell.TH (Q, reportWarning)
import Relude

renderGQLErrors :: GQLErrors -> String
renderGQLErrors = unpack . encode

-- TODO: implement warnings, is nit used
deprecatedEnum :: FieldName -> Ref FieldName -> Maybe Description -> ValidationError
deprecatedEnum typeName Ref {refPosition, refName} reason =
  "the enum value "
    <> msg typeName
    <> "."
    <> msg refName
    <> " is deprecated."
    <> msg (maybe "" (" " <>) reason) `at` refPosition

deprecatedField :: FieldName -> Ref FieldName -> Maybe Description -> ValidationError
deprecatedField typeName Ref {refPosition, refName} reason =
  "the field "
    <> msgValidation typeName
    <> "."
    <> msgValidation refName
    <> " is deprecated."
    <> msgValidation (maybe "" (" " <>) reason) `at` refPosition

gqlWarnings :: [GQLError] -> Q ()
gqlWarnings [] = pure ()
gqlWarnings warnings = traverse_ handleWarning warnings
  where
    handleWarning warning =
      reportWarning ("Morpheus GraphQL Warning: " <> (unpack . encode) warning)
