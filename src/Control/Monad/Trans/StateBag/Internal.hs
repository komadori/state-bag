{-# LANGUAGE DataKinds, KindSignatures, ScopedTypeVariables,
    MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, TypeFamilies,
    TypeOperators, RankNTypes, MagicHash, GeneralizedNewtypeDeriving #-}

module Control.Monad.Trans.StateBag.Internal where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Primitive
import Control.Monad.IO.Class
import Data.Proxy
import GHC.Prim (Any)

-- | Type-class for counting the number of elements in a type-level list.
class ElementCount (a :: [*]) where
    elemCount :: Proxy a -> Int

instance ElementCount '[] where
    elemCount _ = 0

instance (ElementCount xs) => ElementCount (x ': xs) where
    elemCount _ = 1 + elemCount (Proxy :: Proxy xs)

-- | Type-class for finding the index of an element in a type-level list.
class ElementIndex x (xs :: [*]) where
    elemIndex :: Proxy x -> Proxy xs -> Int

instance ElementIndex x (x ': xs) where
    elemIndex _ _ = 0

instance {-# OVERLAPS #-} forall x y xs. (ElementIndex x xs) =>
    ElementIndex x (y ': xs) where
    elemIndex px _ = 1 + elemIndex px (Proxy :: Proxy xs)
