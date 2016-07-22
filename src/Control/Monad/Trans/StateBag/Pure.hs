{-# LANGUAGE DataKinds, KindSignatures, ScopedTypeVariables,
    MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, TypeFamilies,
    TypeOperators, RankNTypes, MagicHash, GeneralizedNewtypeDeriving #-}

-- | State bag monad transformer which runs on any monad stack.
module Control.Monad.Trans.StateBag.Pure (
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

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.StateBag.Internal
import Control.Monad.IO.Class
import Control.Monad.Primitive
import Data.Proxy
import GHC.Prim (Any, unsafeCoerce#)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

newtype BaggerImpl (bag :: [*]) = BaggerImpl [Any]

-- | Monad transformer for building state bags. 
newtype StateBaggerT bag m a = StateBaggerT (StateT (BaggerImpl bag) m a)
    deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans (StateBaggerT bag) where
    lift = StateBaggerT . lift

instance (PrimMonad m) => PrimMonad (StateBaggerT bag m) where
  type PrimState (StateBaggerT bag m) = PrimState m
  primitive = lift . primitive

-- | Run an empty state bagger on top of a monad stack.
runBagger :: (Monad m) => StateBaggerT '[] m a -> m a
runBagger (StateBaggerT s) =
    fmap fst $ runStateT s $ BaggerImpl []

-- | Run a state bagger with one additional item.
addItem :: forall item bag m a. (Monad m) =>
    item -> StateBaggerT (item ': bag) m a -> StateBaggerT bag m a
addItem item (StateBaggerT chain) = StateBaggerT $ do
    (BaggerImpl list) <- get
    (ret, BaggerImpl (_:list')) <- lift $ runStateT chain $
        BaggerImpl (unsafeCoerce# item : list)
    put $ BaggerImpl list'
    return ret

-- | Get the value of the top item in a state bagger.
topItem :: forall item bag m. (Monad m) =>
    StateBaggerT (item ': bag) m item
topItem = StateBaggerT $ do
    (BaggerImpl (item:_)) <- get
    return $ unsafeCoerce# item

-- | Run a state bagger with one additional item and capture the final value of
-- that item on return.
stackItem :: forall item bag m a. (Monad m) =>
    item -> StateBaggerT (item ': bag) m a -> StateBaggerT bag m (a, item)
stackItem item chain =
    addItem item $ liftM2 (,) chain topItem

newtype BagImpl (bag :: [*]) = BagImpl (V.Vector Any)

-- | State bag monad transformer where the state items are represented by the
-- type-level list @bag@.
newtype StateBagT bag m a = StateBagT (StateT (BagImpl bag) m a)
    deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans (StateBagT bag) where
    lift = StateBagT . lift

instance (PrimMonad m) => PrimMonad (StateBagT bag m) where
  type PrimState (StateBagT bag m) = PrimState m
  primitive = lift . primitive

-- | Runs a state bag with the items prepared in a state bagger.
makeBag ::
    forall bag m a. (Monad m, ElementCount bag) =>
    StateBagT bag m a -> StateBaggerT bag m a
makeBag (StateBagT s) = StateBaggerT $ do
    (BaggerImpl list) <- get
    let vlen = elemCount (Proxy :: Proxy bag)
    (ret, BagImpl vec') <- lift $ runStateT s $ BagImpl $ V.fromListN vlen list
    put $ BaggerImpl $ V.toList vec'
    return ret

itemImpl :: forall m item bag.
    (Monad m, ElementIndex item bag) =>
    StateBagT bag m (item, item -> StateBagT bag m ())
{-# INLINE itemImpl #-}
itemImpl = do
    let i = elemIndex (Proxy :: Proxy item) (Proxy :: Proxy bag)
    (BagImpl vec) <- StateBagT get
    let item = unsafeCoerce# $ (V.!) vec i
    let puti item' = StateBagT $ put $ BagImpl $ V.modify (\mvec ->
                        MV.write mvec i $ unsafeCoerce# item') vec
    return (item, puti)

-- | Gets the current value of @item@ from the bag.
getItem :: forall m item bag.
    (Monad m, ElementIndex item bag) =>
    StateBagT (bag :: [*]) m item
getItem = fmap fst itemImpl

-- | Stores a new value of @item@ in the bag.
putItem :: forall m item bag.
    (Monad m, ElementIndex item bag) =>
    item -> StateBagT (bag :: [*]) m ()
putItem item = itemImpl >>= flip snd item

-- | Applies a monadic function to an item in the bag and stores the result.
modifyItemM :: forall m item bag.
    (Monad m, ElementIndex item bag) =>
    (item -> StateBagT bag m item) -> StateBagT bag m ()
modifyItemM f = do
    (item, puti) <- itemImpl
    item' <- f item
    puti item'
