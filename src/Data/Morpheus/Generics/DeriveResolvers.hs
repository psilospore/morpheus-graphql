{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Data.Morpheus.Generics.DeriveResolvers
  ( DeriveResolvers(..)
  , resolversBy
  , resolveBySelection
  ) where

import           Data.Maybe                                 (fromMaybe)
import           Data.Morpheus.Types.Internal.AST.Selection (Selection (..), SelectionRec (..))
import           Data.Morpheus.Types.Internal.Validation    (ResolveIO)
import           Data.Morpheus.Types.Internal.Value         (Value (..))
import           Data.Text                                  (Text, pack)
import           GHC.Generics

-- type D1 = M1 D
-- type C1 = M1 C
-- type S1 = M1 S
-- M1 : Meta-information (constructor names, etc.)
-- D  :Datatype : Class for dataTypes that represent dataTypes
-- C :Constructor :
-- S - Selector: Class for dataTypes that represent records
-- Rep = D1 (...)  (C1 ...) (S1 (...) :+: D1 (...)  (C1 ...) (S1 (...)
unwrapMonadTuple :: Monad m => (Text, m a) -> m (Text, a)
unwrapMonadTuple (text, ioa) = ioa >>= \x -> pure (text, x)

selectResolver :: [(Text, (Text, Selection) -> ResolveIO Value)] -> (Text, Selection) -> ResolveIO (Text, Value)
selectResolver resolvers' (key', selection') =
  case selectionRec selection' of
    SelectionAlias name' aliasSelection' ->
      unwrapMonadTuple (key', lookupResolver name' (selection' {selectionRec = aliasSelection'}))
    _ -> unwrapMonadTuple (key', lookupResolver key' selection')
  where
    lookupResolver resolverKey' sel = (fromMaybe (\_ -> pure Null) $ lookup resolverKey' resolvers') (key', sel)

resolveBySelection :: [(Text, Selection)] -> [(Text, (Text, Selection) -> ResolveIO Value)] -> ResolveIO Value
resolveBySelection selection resolvers = Object <$> mapM (selectResolver resolvers) selection

resolversBy :: (Generic a, DeriveResolvers (Rep a)) => a -> [(Text, (Text, Selection) -> ResolveIO Value)]
resolversBy = deriveResolvers "" . from

class DeriveResolvers f where
  deriveResolvers :: Text -> f a -> [(Text, (Text, Selection) -> ResolveIO Value)]

instance DeriveResolvers U1 where
  deriveResolvers _ _ = []

instance (Selector s, DeriveResolvers f) => DeriveResolvers (M1 S s f) where
  deriveResolvers _ m@(M1 src) = deriveResolvers (pack $ selName m) src

instance DeriveResolvers f => DeriveResolvers (M1 D c f) where
  deriveResolvers key' (M1 src) = deriveResolvers key' src

instance DeriveResolvers f => DeriveResolvers (M1 C c f) where
  deriveResolvers key' (M1 src) = deriveResolvers key' src

instance (DeriveResolvers f, DeriveResolvers g) => DeriveResolvers (f :*: g) where
  deriveResolvers meta (a :*: b) = deriveResolvers meta a ++ deriveResolvers meta b