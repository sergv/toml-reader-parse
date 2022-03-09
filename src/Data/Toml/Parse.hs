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
{-# LANGUAGE FlexibleContexts           #-}
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
  , AtomicTomlError(..)
  , (<?>)
  , L
  , extract
  , TomlParse(..)
  , FromToml(..)
  , Index(..)
  , (.!=)
  , pTable
  , pKey
  , pStr
  , pStrL
  , pBool
  , pInt
  , pIntL
  , pDouble
  , pDoubleL
  , pDatetime
  , pDatetimeL
  , pTArray
  , pArray
  , pCases

  , ppToml
  ) where

import Control.Applicative
import Control.Monad.Except

import Data.Bifunctor
import Data.DList (DList)
import qualified Data.DList as DL
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Traversable
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Void (Void, vacuous)
import GHC.Generics (Generic)
import Prettyprinter
import Prettyprinter.Combinators
import Text.Toml hiding (TomlError)

import Unsafe.Coerce

data TomlType
  = TTable
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

data TomlPath
  = PathIndex !Int
  | PathKey !Text
  | PathOther !Text
  deriving (Eq, Ord, Show, Generic)

instance Pretty TomlPath where
  pretty = \case
    PathIndex n     -> "In array element" <+> pretty n
    PathKey str     -> "In table key" <+> squotes (pretty str)
    PathOther thing -> "While parsing" <+> pretty thing

data AtomicTomlError
  = UnexpectedType
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


data TomlError
  = ErrorEmpty
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
    ErrorAnd x y     -> "AND" ## align (vsep $ map pretty $ toList $ collectConjuctions x y)
    ErrorOr  x y     -> "OR"  ## align (vsep $ map pretty $ toList $ collectDisjunctions x y)
    ErrorPrefix ps e -> foldr (\p acc -> pretty p ## acc) (pretty e) ps
    where
      collectConjuctions :: TomlError -> TomlError -> DList TomlError
      collectConjuctions (ErrorAnd a b) (ErrorAnd c d) = collectConjuctions a b <> collectConjuctions c d
      collectConjuctions (ErrorAnd a b) c              = collectConjuctions a b <> DL.singleton c
      collectConjuctions a              (ErrorAnd c d) = DL.singleton a <> collectConjuctions c d
      collectConjuctions a              c              = DL.fromList [a, c]

      collectDisjunctions :: TomlError -> TomlError -> DList TomlError
      collectDisjunctions (ErrorOr a b) (ErrorOr c d) = collectDisjunctions a b <> collectDisjunctions c d
      collectDisjunctions (ErrorOr a b) c             = collectDisjunctions a b <> DL.singleton c
      collectDisjunctions a             (ErrorOr c d) = DL.singleton a <> collectDisjunctions c d
      collectDisjunctions a             c             = DL.fromList [a, c]

-- NB order of constructors is important
data IsCommitted = Uncommitted | Committed
  deriving (Eq, Ord, Show, Enum, Bounded)

instance Semigroup IsCommitted where
  {-# INLINE (<>) #-}
  (<>) = max

newtype Validation a = Validation
  { unValidation :: Either (IsCommitted, TomlError) a }
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
  {-# NOINLINE (<*>) #-}
  (<*>) vf'@(Validation vf) vx'@(Validation vx) =
    case (vf, vx) of
      (Left (cf, ef), Left (cx, ex)) -> Validation $ Left (cf <> cx, zipErrors ErrorAnd ef ex)
      (Left _,        _)             -> unsafeCoerce vf'
      (_,             Left _)        -> unsafeCoerce vx'
      (Right f,       Right x)       -> Validation $ Right $ f x

instance Alternative Validation where
  {-# INLINE empty #-}
  empty = Validation $ Left (Uncommitted, ErrorEmpty)
  {-# NOINLINE (<|>) #-}
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

infixl 9 <?>

(<?>) :: L a -> Text -> L a
(<?>) (L env x) y = L (inside (PathOther y) env) x

instance TomlParse Parser where
  {-# NOINLINE throwParseError #-}
  throwParseError (L env _) err = Parser $ Validation $ Left (Uncommitted, err')
    where
      err' = case reverse $ unParseEnv env of
        []     -> ErrorAtomic err
        p : ps -> ErrorPrefix (p :| ps) $ ErrorAtomic err

runParser :: a -> (L a -> Parser b) -> Either (Doc Void) b
runParser x f
  = bimap (("Error while parsing:" ##) . pretty . snd) id
  $ unValidation
  $ unParser
  $ f
  $ L (ParseEnv []) x

data L a = L ParseEnv a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

{-# INLINE extract #-}
extract :: L a -> a
extract (L _ x) = x

{-# INLINE inside #-}
inside :: TomlPath -> ParseEnv -> ParseEnv
inside x (ParseEnv xs) = ParseEnv (x : xs)

class (Applicative m, Alternative m) => TomlParse m where
  throwParseError :: L b -> AtomicTomlError -> m a

class FromToml a b where
  fromToml :: L a -> Parser b

instance FromToml a (L a) where
  {-# INLINE fromToml #-}
  fromToml = pure

instance FromToml a a where
  {-# INLINE fromToml #-}
  fromToml = pure . extract

instance FromToml Node String where
  {-# INLINE fromToml #-}
  fromToml = fmap T.unpack . pStr

instance FromToml Node Text where
  {-# INLINE fromToml #-}
  fromToml = pStr

instance FromToml Node Bool where
  {-# INLINE fromToml #-}
  fromToml = pBool

instance FromToml Node Int where
  {-# INLINE fromToml #-}
  fromToml = pInt

instance FromToml Node Double where
  {-# INLINE fromToml #-}
  fromToml = pDouble

instance FromToml Node UTCTime where
  {-# INLINE fromToml #-}
  fromToml = pDatetime

-- instance FromToml Node a => FromToml Node [a] where
--   {-# INLINE fromToml #-}
--   fromToml = pArray >=> traverse fromToml . toList

instance (Ord k, FromToml Text k, FromToml Node v) => FromToml Node (Map k v) where
  -- {-# INLINE fromToml #-}
  fromToml = pTable >=> fromToml


instance (Ord k, FromToml Text k, FromToml Node v) => FromToml Table (Map k v) where
  -- {-# INLINE fromToml #-}
  fromToml (L env y) = do
    ys <- for (HM.toList y) $ \(k, v) ->
      (,)
        <$> fromToml (L env k)
        <*> fromToml (L (inside (PathKey k) env) v)
    pure $ M.fromList ys

instance FromToml Node a => FromToml Node (Vector a) where
  -- {-# INLINE fromToml #-}
  fromToml = pArray >=> traverse fromToml

instance FromToml Node a => FromToml Node (NonEmpty a) where
  -- {-# INLINE fromToml #-}
  fromToml x = do
    ys <- pArray x
    case toList ys of
      []     -> throwParseError x $ OtherError "Expected a non-empty list"
      z : zs -> (:|) <$> fromToml z <*> traverse fromToml zs

infixl 5 .:, .:?, .!=

class Index a where
  (.:)  :: FromToml Node b => a -> Text -> Parser b
  (.:?) :: FromToml Node b => a -> Text -> Parser (Maybe b)

instance Index (L Table) where
  {-# INLINE (.:)  #-}
  {-# INLINE (.:?) #-}
  (.:)  x key = pKey key x >>= fromToml
  (.:?) x key = traverse fromToml $ pKeyMaybe key x

instance Index (L Node) where
  {-# INLINE (.:)  #-}
  {-# INLINE (.:?) #-}
  (.:)  x key = pTable x >>= pKey key >>= fromToml
  (.:?) x key = pTable x >>= traverse fromToml . pKeyMaybe key

instance a ~ L Node => Index (Parser a) where
  {-# INLINE (.:)  #-}
  {-# INLINE (.:?) #-}
  (.:)  x key = x >>= pTable >>= pKey key >>= fromToml
  (.:?) x key = x >>= pTable >>= traverse fromToml . pKeyMaybe key

-- | Assign default value to a parser that produces 'Maybe'. Typically used together with '.:?':
--
-- > foo .:? "bar" .!= 10
{-# INLINE (.!=) #-}
(.!=) :: Functor m => m (Maybe a) -> a -> m a
(.!=) action def = fromMaybe def <$> action

pTable :: TomlParse m => L Node -> m (L Table)
pTable = \case
  L env (VTable x)   -> pure $ L env x
  other@(L _ other') -> throwParseError other $ UnexpectedType TTable other'

pKey :: TomlParse m => Text -> L Table -> m (L Node)
pKey key tab'@(L _ tab) = case pKeyMaybe key tab' of
  Just x  -> pure x
  Nothing -> throwParseError tab' $ MissingKey key tab

pKeyMaybe :: Text -> L Table -> Maybe (L Node)
pKeyMaybe key (L env tab) = case HM.lookup key tab of
  Just x  -> Just $ L (inside (PathKey key) env) x
  Nothing -> Nothing

pStr :: TomlParse m => L Node -> m Text
pStr = fmap extract . pStrL

pStrL :: TomlParse m => L Node -> m (L Text)
pStrL = \case
  L env (VString x)  -> pure $ L env x
  other@(L _ other') -> throwParseError other $ UnexpectedType TString other'

pBool :: TomlParse m => L Node -> m Bool
pBool = \case
  L _ (VBoolean x)    -> pure x
  other@ (L _ other') -> throwParseError other $ UnexpectedType TBoolean other'

pInt :: TomlParse m => L Node -> m Int
pInt = fmap extract . pIntL

pIntL :: TomlParse m => L Node -> m (L Int)
pIntL = \case
  L env (VInteger x) -> pure $ L env $ fromIntegral x
  other@(L _ other') -> throwParseError other $ UnexpectedType TInteger other'

pDouble :: TomlParse m => L Node -> m Double
pDouble = fmap extract . pDoubleL

pDoubleL :: TomlParse m => L Node -> m (L Double)
pDoubleL = \case
  L env (VFloat x)   -> pure $ L env x
  other@(L _ other') -> throwParseError other $ UnexpectedType TFloat other'

pDatetime :: TomlParse m => L Node -> m UTCTime
pDatetime = fmap extract . pDatetimeL

pDatetimeL :: TomlParse m => L Node -> m (L UTCTime)
pDatetimeL = \case
  L env (VDatetime x) -> pure $ L env x
  other@(L _ other')  -> throwParseError other $ UnexpectedType TDatetime other'

pTArray :: TomlParse m => L Node -> m (Vector (L Table))
pTArray = \case
  L env (VTArray x)  -> pure $ (\(n, x') -> L (inside (PathIndex n) env) x') <$> V.indexed x
  other@(L _ other') -> throwParseError other $ UnexpectedType TTArray other'

pArray :: TomlParse m => L Node -> m (Vector (L Node))
pArray = \case
  L env (VArray x)   -> pure $ (\(n, x') -> L (inside (PathIndex n) env) x') <$> V.indexed x
  other@(L _ other') -> throwParseError other $ UnexpectedType TArray other'

{-# INLINE pCases #-}
pCases :: (Ord k, FromToml Node k, Pretty k) => Map k v -> L Node -> Parser v
pCases env = \x -> do
  k <- fromToml x
  case M.lookup k env of
    Just v  -> pure v
    Nothing -> throwParseError x $ OtherError $
      "Unexpected value" <+> squotes (pretty k) <> "." <+>
      "Expected one of" <+> vsep (punctuate "," (map pretty (M.keys env)))
