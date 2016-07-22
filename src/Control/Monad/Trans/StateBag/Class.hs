{-# LANGUAGE DataKinds, KindSignatures, ScopedTypeVariables,
    MultiParamTypeClasses, FlexibleContexts, FlexibleInstances,
    TypeFamilies, TypeOperators, RankNTypes #-}

-- | Abstraction over the primitive and pure versions of StateBagT.
module Control.Monad.Trans.StateBag.Class (
    StateBagMonad(
        Bag,
        BagBase,
        getItem,
        putItem,
        modifyItemM),
    ElementCount(),
    ElementIndex()
) where

import Control.Monad.Primitive
import Control.Monad.Trans.StateBag.Internal
import qualified Control.Monad.Trans.StateBag.Pure as Pure
import qualified Control.Monad.Trans.StateBag.Primitive as Prim

-- | Type-class which abstract over the primitive and pure versions of
-- StateBagT.
class (Monad m) => StateBagMonad (m :: * -> *) where
    type Bag m :: [*]
    type BagBase m :: * -> *
    -- | Gets the current value of @item@ from the bag.
    getItem :: (ElementIndex item (Bag m)) => m item
    -- | Stores a new value of @item@ in the bag.
    putItem :: (ElementIndex item (Bag m)) => item -> m ()
    -- | Applies a monadic function to an item in the bag and stores the result.
    modifyItemM :: (ElementIndex item (Bag m)) => (item -> m item) -> m ()
    modifyItemM f = getItem >>= f >>= putItem

instance (Monad m) => StateBagMonad (Pure.StateBagT bag m) where
    type Bag (Pure.StateBagT bag m) = bag
    type BagBase (Pure.StateBagT bag m) = m
    getItem = Pure.getItem
    putItem = Pure.putItem
    modifyItemM = Pure.modifyItemM

instance (PrimMonad m) => StateBagMonad (Prim.StateBagT bag m) where
    type Bag (Prim.StateBagT bag m) = bag
    type BagBase (Prim.StateBagT bag m) = m
    getItem = Prim.getItem
    putItem = Prim.putItem
    modifyItemM = Prim.modifyItemM
