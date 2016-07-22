{-# LANGUAGE DataKinds, KindSignatures, ScopedTypeVariables,
    MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, TypeFamilies,
    TypeOperators, RankNTypes, MagicHash, GeneralizedNewtypeDeriving #-}

-- | State bag monad transformer which runs on a PrimMonad stack.
module Control.Monad.Trans.StateBag.Primitive (
    StateBaggerT,
    runBagger,
    addItem,
    topItem,
    stackItem,
    StateBagT,
    makeBag,
    getItem,
    putItem,
    modifyItemM,
    ElementCount(),
    ElementIndex(),
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.StateBag.Internal
import Control.Monad.IO.Class
import Control.Monad.Primitive
import Data.Proxy
import GHC.Prim (Any, unsafeCoerce#)
import qualified Data.Vector.Mutable as V

newtype BagImpl s (bag :: [*]) = BagImpl (V.MVector s Any)

-- | Monad transformer for building state bags. 
newtype StateBaggerT full (bag :: [*]) m a
    = StateBaggerT (ReaderT (BagImpl (PrimState m) full) m a)
    deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans (StateBaggerT full bag) where
    lift = StateBaggerT . lift

instance (PrimMonad m) => PrimMonad (StateBaggerT full bag m) where
  type PrimState (StateBaggerT full bag m) = PrimState m
  primitive = lift . primitive

-- | Run an empty state bagger on top of a monad stack.
runBagger :: forall full m a. (PrimMonad m, ElementCount full) =>
    StateBaggerT full '[] m a -> m a
runBagger (StateBaggerT r) = do
    vec <- V.new $ elemCount (Proxy :: Proxy full)
    runReaderT r $ BagImpl vec

-- | Run a state bagger with one additional item.
addItem :: forall item full bag m a. (PrimMonad m, ElementIndex item full) =>
    item -> StateBaggerT full (item ': bag) m a -> StateBaggerT full bag m a
addItem item (StateBaggerT chain) = StateBaggerT $ do
    (BagImpl vec) <- ask
    V.write vec (elemIndex (Proxy :: Proxy item) (Proxy :: Proxy full)) $
        unsafeCoerce# item
    lift $ runReaderT chain $ BagImpl vec

-- | Get the value of the top item in a state bagger.
topItem :: forall item full bag m. (PrimMonad m, ElementIndex item full) =>
    StateBaggerT full (item ': bag) m item
topItem = StateBaggerT $ do
    (BagImpl vec) <- ask
    fmap unsafeCoerce# $ V.read vec $
        elemIndex (Proxy :: Proxy item) (Proxy :: Proxy full)

-- | Run a state bagger with one additional item and capture the final value of
-- that item on return.
stackItem :: forall item full bag m a. (PrimMonad m, ElementIndex item full) =>
    item -> StateBaggerT full (item ': bag) m a ->
    StateBaggerT full bag m (a, item)
stackItem item chain =
    addItem item $ liftM2 (,) chain topItem

-- | State bag monad transformer where the state items are represented by the
-- type-level list @bag@.
newtype StateBagT bag m a = StateBagT (ReaderT (BagImpl (PrimState m) bag) m a)
    deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans (StateBagT bag) where
    lift = StateBagT . lift

instance (PrimMonad m) => PrimMonad (StateBagT bag m) where
  type PrimState (StateBagT bag m) = PrimState m
  primitive = lift . primitive

-- | Runs a state bag with the items prepared in a state bagger.
makeBag ::
    forall bag m a. (PrimMonad m, ElementCount bag) =>
    StateBagT bag m a -> StateBaggerT bag bag m a
makeBag (StateBagT r) = StateBaggerT r

itemImpl :: forall m item bag.
    (PrimMonad m, ElementIndex item bag) =>
    StateBagT bag m (StateBagT bag m item, item -> StateBagT bag m ())
{-# INLINE itemImpl #-}
itemImpl = do
    let i = elemIndex (Proxy :: Proxy item) (Proxy :: Proxy bag)
    (BagImpl vec) <- StateBagT ask
    let geti = fmap unsafeCoerce# $ V.read vec i
    let puti item = V.write vec i $ unsafeCoerce# item
    return (geti, puti)

-- | Gets the current value of @item@ from the bag.
getItem :: forall m item bag.
    (PrimMonad m, ElementIndex item bag) =>
    StateBagT bag m item
{-# INLINE getItem #-}
getItem = itemImpl >>= fst

-- | Stores a new value of @item@ in the bag.
putItem :: forall m item bag.
    (PrimMonad m, ElementIndex item bag) =>
    item -> StateBagT bag m ()
{-# INLINE putItem #-}
putItem item = itemImpl >>= flip snd item

-- | Applies a monadic function to an item in the bag and stores the result.
modifyItemM :: forall m item bag.
    (PrimMonad m, ElementIndex item bag) =>
    (item -> StateBagT bag m item) -> StateBagT bag m ()
{-# INLINE modifyItemM #-}
modifyItemM f = do
    (get, put) <- itemImpl
    item <- get
    item' <- f item
    put item'
