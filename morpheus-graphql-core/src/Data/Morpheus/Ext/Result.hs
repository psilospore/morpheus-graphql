{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Ext.Result
  ( Eventless,
    Result (..),
    ResultT (..),
    unpackEvents,
    mapEvent,
    cleanEvents,
    PushEvents (..),
    resultOr,
    sortErrors,
    toEither,
    ValidationResult,
  )
where

import Control.Monad.Except (MonadError (..))
import Data.Morpheus.Types.Internal.AST.Error
  ( GQLError (..),
    ValidationError,
  )
import Relude

type Eventless = Result () GQLError

type ValidationResult = Result () ValidationError

-- EVENTS
class PushEvents e m where
  pushEvents :: [e] -> m ()

unpackEvents :: Result event er a -> [event]
unpackEvents Success {events} = events
unpackEvents _ = []

--
-- Result
--
--
data Result events err a
  = Success {result :: a, warnings :: [err], events :: [events]}
  | Failure {errors :: [err]}
  deriving (Functor)

instance Applicative (Result e er) where
  pure x = Success x [] []
  Success f w1 e1 <*> Success x w2 e2 = Success (f x) (w1 <> w2) (e1 <> e2)
  Failure e1 <*> Failure e2 = Failure (e1 <> e2)
  Failure e <*> Success _ w _ = Failure (e <> w)
  Success _ w _ <*> Failure e = Failure (e <> w)

instance Monad (Result e er) where
  return = pure
  Success v w1 e1 >>= fm = case fm v of
    (Success x w2 e2) -> Success x (w1 <> w2) (e1 <> e2)
    (Failure e) -> Failure (e <> w1)
  Failure e >>= _ = Failure e

instance Bifunctor (Result ev) where
  bimap f g Success {..} = Success {warnings = f <$> warnings, result = g result, ..}
  bimap f _ Failure {..} = Failure (f <$> errors)

instance MonadError er (Result ev er) where
  throwError = Failure . pure
  catchError = undefined

instance PushEvents events (Result events er) where
  pushEvents events = Success {result = (), warnings = [], events}

resultOr :: ([err] -> a') -> (a -> a') -> Result e err a -> a'
resultOr _ f (Success x _ _) = f x
resultOr f _ (Failure e) = f e

sortErrors :: Result e GQLError a -> Result e GQLError a
sortErrors (Failure errors) = Failure (sortOn locations errors)
sortErrors x = x

-- ResultT
newtype ResultT event (m :: * -> *) a = ResultT
  { runResultT :: m (Result event GQLError a)
  }
  deriving (Functor)

instance Applicative m => Applicative (ResultT event m) where
  pure = ResultT . pure . pure
  ResultT app1 <*> ResultT app2 = ResultT $ liftA2 (<*>) app1 app2

instance Monad m => Monad (ResultT event m) where
  return = pure
  (ResultT m1) >>= mFunc = ResultT $ do
    result1 <- m1
    case result1 of
      Failure errors -> pure $ Failure errors
      Success value1 w1 e1 -> do
        result2 <- runResultT (mFunc value1)
        case result2 of
          Failure errors -> pure $ Failure (errors <> w1)
          Success v2 w2 e2 -> pure $ Success v2 (w1 <> w2) (e1 <> e2)

instance MonadTrans (ResultT event) where
  lift = ResultT . fmap pure

instance Monad m => MonadError GQLError (ResultT event m) where
  throwError = ResultT . pure . throwError
  catchError = undefined

instance Applicative m => PushEvents event (ResultT event m) where
  pushEvents = ResultT . pure . pushEvents

cleanEvents ::
  Functor m =>
  ResultT e m a ->
  ResultT e' m a
cleanEvents resT = ResultT $ replace <$> runResultT resT
  where
    replace (Success v w _) = Success v w []
    replace (Failure e) = Failure e

mapEvent ::
  Monad m =>
  (e -> e') ->
  ResultT e m value ->
  ResultT e' m value
mapEvent func (ResultT ma) = ResultT $ mapEv <$> ma
  where
    mapEv Success {result, warnings, events} =
      Success {result, warnings, events = fmap func events}
    mapEv (Failure err) = Failure err

toEither :: Result e err b -> Either [err] b
toEither = resultOr Left Right
