{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Error.Utils
  ( errorMessage,
    globalErrorMessage,
  )
where

import Data.Morpheus.Types.Internal.AST.Base
  ( Message,
    Position (..),
  )
import Data.Morpheus.Types.Internal.AST.Error
  ( GQLError (..),
  )
import Relude hiding (ByteString)

errorMessage :: Position -> Message -> [GQLError]
errorMessage position message =
  [ GQLError
      { message,
        locations = [position],
        extensions = Nothing
      }
  ]

{-# DEPRECATED globalErrorMessage "use validation errors" #-}
globalErrorMessage :: Message -> [GQLError]
globalErrorMessage message =
  [ GQLError
      { message,
        locations = [],
        extensions = Nothing
      }
  ]
