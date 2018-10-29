----------------------------------------------------------------------------
-- |
-- Module      :  Data.Toml.Parse
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD-2 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Data.Toml.Parse
  ( Node(..)
  , Parser
  , runParser
  , L
  , TomlParse(..)
  , FromToml(..)
  , Index(..)
  , (.!=)
  , pTable
  , pKey
  , pStr
  , pInt
  , pDouble
  , pDatetime
  , pTArray
  , pArray
  ) where

import Control.Applicative
import Control.Monad.Except

import Data.Bifunctor
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Combinators
import Data.Time
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Void (Void, vacuous)
import GHC.Generics (Generic)
import Text.Toml hiding (TomlError)

import Unsafe.Coerce

data TomlType =
    TTable
  | TTArray
  | TString
  | TInteger
  | TFloat
  | TBoolean
  | TDatetime
  | TArray
  deriving (Eq, Ord, Show, Generic)

getType :: Node -> TomlType
getType = \case
  VTable{}    -> TTable
  VTArray{}   -> TTArray
  VString{}   -> TString
  VInteger{}  -> TInteger
  VFloat{}    -> TFloat
  VBoolean{}  -> TBoolean
  VDatetime{} -> TDatetime
  VArray{}    -> TArray

ppTomlType :: TomlType -> (Doc ann, Doc ann)
ppTomlType = \case
  TTable    -> ("a",  "table")
  TTArray   -> ("a",  "table array")
  TString   -> ("a",  "string")
  TInteger  -> ("an", "integer")
  TFloat    -> ("a",  "float")
  TBoolean  -> ("a",  "boolean")
  TDatetime -> ("a",  "datetime")
  TArray    -> ("an", "array")

data TomlPath =
    PathIndex !Int
  | PathKey !Text
  deriving (Eq, Ord, Show, Generic)

instance Pretty TomlPath where
  pretty = \case
    PathIndex n -> "In array element" <+> pretty n
    PathKey str -> "In table key" <+> squotes (pretty str)

data AtomicTomlError =
    UnexpectedType
      !TomlType -- ^ Expected
      Node      -- ^ Got
  | MissingKey !Text Table
  | IndexOutOfBounds !Int Node
  | OtherError (Doc Void)
  deriving (Show, Generic)

ppToml :: Node -> Doc ann
ppToml = \case
  VTable    x  -> ppHashMapWith pretty ppToml x
  VTArray   xs -> ppVectorWith (ppHashMapWith pretty ppToml) xs
  VString   x  -> pretty x
  VInteger  x  -> pretty x
  VFloat    x  -> pretty x
  VBoolean  x  -> pretty x
  VDatetime x  -> pretty $ formatTime defaultTimeLocale (iso8601DateFormat Nothing) x
  VArray    xs -> ppVectorWith ppToml xs

instance Pretty AtomicTomlError where
  pretty = \case
    UnexpectedType expected got ->
      "Expected to find" <+> article <+> typ <+> "but found" <+> article' <+> typ' <> "." <+>
      "Node:" ## ppToml got
      where
        (article,  typ)  = ppTomlType expected
        (article', typ') = ppTomlType $ getType got
    MissingKey key tab          -> "Missing key" <+> squotes (pretty key) <+> "in table:" ## ppHashMapWith pretty ppToml tab
    IndexOutOfBounds ix node    -> "Index" <+> pretty ix <+> "is out of bounds in array:" ## ppToml node
    OtherError err              -> "Other error:" ## vacuous err


data TomlError =
    ErrorEmpty
  | ErrorAtomic !AtomicTomlError
  -- | Invariant: children of ErrorAnd never share common prefix.
  | ErrorAnd TomlError TomlError
  -- | Invariant: children of ErrorOr never share common prefix.
  | ErrorOr TomlError TomlError
  | ErrorPrefix (NonEmpty TomlPath) TomlError
  deriving (Show, Generic)

instance Pretty TomlError where
  pretty = \case
    ErrorEmpty       -> "Control.Applicative.empty"
    ErrorAtomic err  -> pretty err
    ErrorAnd x y     -> "AND" ## align (vsep [pretty x, pretty y])
    ErrorOr  x y     -> "OR" ## align (vsep [pretty x, pretty y])
    ErrorPrefix ps e -> foldr (\p acc -> pretty p ## acc) (pretty e) ps

data IsCommitted = Uncommitted | Committed
  deriving (Eq, Ord, Show, Enum, Bounded)

instance Semigroup IsCommitted where
  {-# INLINE (<>) #-}
  (<>) = max

newtype Validation a = Validation { unValidation :: Either (IsCommitted, TomlError) a }
  deriving (Functor)

zipErrors
  :: (TomlError -> TomlError -> TomlError)
  -> TomlError
  -> TomlError
  -> TomlError
zipErrors f x y = case commonPrefix x y of
  Nothing               -> f x y
  Just (common, x', y') ->
    ErrorPrefix common (f x' y')

commonPrefix
  :: TomlError
  -> TomlError
  -> Maybe (NonEmpty TomlPath, TomlError, TomlError)
commonPrefix x y = case (x, y) of
  (ErrorPrefix px x', ErrorPrefix py y') ->
    flip fmap (go px py) $ \(common, px', py') ->
      let prefix []       err = err
          prefix (p : ps) err = ErrorPrefix (p :| ps) err
      in (common, prefix px' x', prefix py' y')
  _ -> Nothing
  where
    go :: NonEmpty TomlPath -> NonEmpty TomlPath -> Maybe (NonEmpty TomlPath, [TomlPath], [TomlPath])
    go xs ys =
      case go' [] (toList xs) (toList ys) of
        (c : cs, xs', ys') -> Just (c :| cs, xs', ys')
        _ -> Nothing
    go' :: Eq a => [a] -> [a] -> [a] -> ([a], [a], [a])
    go' common (a : as) (b : bs)
      | a == b = go' (a : common) as bs
    go' common as bs = (reverse common, as, bs)

instance Applicative Validation where
  {-# INLINE pure #-}
  pure = Validation . pure
  (<*>) vf'@(Validation vf) vx'@(Validation vx) =
    case (vf, vx) of
      (Left (cf, ef), Left (cx, ex)) -> Validation $ Left (cf <> cx, zipErrors ErrorAnd ef ex)
      (Left _,        _)             -> unsafeCoerce vf'
      (_,             Left _)        -> unsafeCoerce vx'
      (Right f,       Right x)       -> Validation $ Right $ f x

instance Alternative Validation where
  empty = Validation $ Left (Uncommitted, ErrorEmpty)
  (<|>) x'@(Validation x) y'@(Validation y) =
    case (x, y) of
      (Right _,       _)             -> x'
      (_,             Right _)       -> y'
      (Left (cf, ef), Left (cx, ex)) ->
        case (cf, cx) of
          (Committed,   Uncommitted) -> x'
          (Uncommitted, Committed)   -> y'
          _                          -> Validation $ Left (cf <> cx, zipErrors ErrorOr ef ex)

instance Monad Validation where
  {-# INLINE (>>=) #-}
  {-# INLINE (>>)  #-}
  (>>=) x'@(Validation x) f =
    case x of
      Left  _ -> unsafeCoerce x'
      Right y -> commit $ f y
    where
      commit (Validation (Left (_, err))) = Validation $ Left (Committed, err)
      commit z@(Validation (Right _))     = z
  (>>) = (*>)

instance MonadPlus Validation

newtype ParseEnv = ParseEnv { unParseEnv :: [TomlPath] }
  deriving (Eq, Ord, Show)

newtype Parser a = Parser
  { unParser :: Validation a }
  deriving (Functor, Applicative, Alternative, MonadPlus)

instance Monad Parser where
  {-# INLINE (>>=) #-}
  {-# INLINE (>>)  #-}
  (>>=) (Parser x) f = Parser $ do
    x' <- x
    unParser $ f x'
  (>>) = (*>)

instance TomlParse Parser where
  throwParseError env err = Parser $ Validation $ Left (Uncommitted, err')
    where
      err' = case unParseEnv env of
        []     -> ErrorAtomic err
        p : ps -> ErrorPrefix (p :| ps) $ ErrorAtomic err

runParser :: Node -> (L Node -> Parser a) -> Either (Doc Void) a
runParser x f
  = bimap (("Error while parsing:" ##) . pretty . snd) id
  $ unValidation
  $ unParser
  $ f
  $ L (ParseEnv []) x

data L a = L ParseEnv a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

{-# INLINE inside #-}
inside :: TomlPath -> ParseEnv -> ParseEnv
inside x (ParseEnv xs) = ParseEnv (x : xs)

class (Applicative m, Alternative m) => TomlParse m where
  throwParseError :: ParseEnv -> AtomicTomlError -> m a

class FromToml a where
  fromToml :: (TomlParse m, Monad m) => L Node -> m a

instance FromToml Node where
  {-# INLINE fromToml #-}
  fromToml (L _ x) = pure x

instance FromToml Text where
  {-# INLINE fromToml #-}
  fromToml = pStr

instance FromToml Int where
  {-# INLINE fromToml #-}
  fromToml = pInt

instance FromToml Double where
  {-# INLINE fromToml #-}
  fromToml = pDouble

instance FromToml UTCTime where
  {-# INLINE fromToml #-}
  fromToml = pDatetime

instance FromToml a => FromToml (Vector a) where
  {-# INLINE fromToml #-}
  fromToml = pArray >=> traverse fromToml

instance FromToml a => FromToml (NonEmpty a) where
  {-# INLINE fromToml #-}
  fromToml x@(L env _) = do
    ys <- pArray x
    case toList ys of
      []     -> throwParseError env $ OtherError "Expected non-empty list"
      z : zs -> (:|) <$> fromToml z <*> traverse fromToml zs

infixl 5 .:, .:?, .!=

class Index a m where
  (.:)  :: FromToml b => a -> Text -> m b
  (.:?) :: FromToml b => a -> Text -> m (Maybe b)

instance (TomlParse m, Monad m) => Index (L Table) m where
  {-# INLINE (.:)  #-}
  {-# INLINE (.:?) #-}
  (.:)  x key = pKey key x >>= fromToml
  (.:?) x key = traverse fromToml $ pKeyMaybe key x

instance (TomlParse m, Monad m) => Index (L Node) m where
  {-# INLINE (.:)  #-}
  {-# INLINE (.:?) #-}
  (.:)  x key = pTable x >>= pKey key >>= fromToml
  (.:?) x key = pTable x >>= traverse fromToml . pKeyMaybe key

instance (TomlParse m, Monad m, a ~ L Node) => Index (m a) m where
  {-# INLINE (.:)  #-}
  {-# INLINE (.:?) #-}
  (.:)  x key = x >>= pTable >>= pKey key >>= fromToml
  (.:?) x key = x >>= pTable >>= traverse fromToml . pKeyMaybe key

(.!=) :: Functor m => m (Maybe a) -> a -> m a
(.!=) action def = fromMaybe def <$> action

pTable :: TomlParse m => L Node -> m (L Table)
pTable = \case
  L env (VTable x) -> pure $ L env x
  L env other      -> throwParseError env $ UnexpectedType TTable other

pKey :: TomlParse m => Text -> L Table -> m (L Node)
pKey key tab'@(L env tab) = case pKeyMaybe key tab' of
  Just x  -> pure x
  Nothing -> throwParseError env $ MissingKey key tab

pKeyMaybe :: Text -> L Table -> Maybe (L Node)
pKeyMaybe key (L env tab) = case HM.lookup key tab of
  Just x  -> Just $ L (inside (PathKey key) env) x
  Nothing -> Nothing

pStr :: TomlParse m => L Node -> m Text
pStr = \case
  L _   (VString x) -> pure x
  L env other       -> throwParseError env $ UnexpectedType TString other

pInt :: TomlParse m => L Node -> m Int
pInt = \case
  L _   (VInteger x) -> pure $ fromIntegral x
  L env other        -> throwParseError env $ UnexpectedType TInteger other

pDouble :: TomlParse m => L Node -> m Double
pDouble = \case
  L _   (VFloat x) -> pure x
  L env other      -> throwParseError env $ UnexpectedType TFloat other

pDatetime :: TomlParse m => L Node -> m UTCTime
pDatetime = \case
  L _   (VDatetime x) -> pure x
  L env other         -> throwParseError env $ UnexpectedType TDatetime other

pTArray :: TomlParse m => L Node -> m (Vector (L Table))
pTArray = \case
  L env (VTArray x) -> pure $ (\(n, x') -> L (inside (PathIndex n) env) x') <$> V.indexed x
  L env other       -> throwParseError env $ UnexpectedType TTArray other

pArray :: TomlParse m => L Node -> m (Vector (L Node))
pArray = \case
  L env (VArray x) -> pure $ (\(n, x') -> L (inside (PathIndex n) env) x') <$> V.indexed x
  L env other      -> throwParseError env $ UnexpectedType TArray other
