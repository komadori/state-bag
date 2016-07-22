{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, RankNTypes #-}

module Main where

import Test.Hspec
import Data.Functor.Identity
import Control.Monad.Trans.StateBag.Class
import Control.Monad.ST
import qualified Control.Monad.Trans.StateBag.Pure as Pure
import qualified Control.Monad.Trans.StateBag.Primitive as Prim

main :: IO ()
main = hspec $ do
  let abc0 = (1,2,3)
      abc1 = (4,5,6)
      abcF = ((+10), (+20), (+30))
      abc2 = (11,22,33)
      check r (a, b) = r == a && r == b
  describe "Pure.StateBag" $ do
      it "can get" $
          check abc0 $ runPureBag abc0 getABC
      it "can put" $
          check abc1 $ runPureBag abc0 $ putABC abc1 >> getABC
      it "can modify" $
          check abc2 $ runPureBag abc0 $ modifyABC abcF >> getABC
  describe "Prim.StateBag" $ do
      it "can get" $
          check abc0 $ runPrimBag abc0 getABC
      it "can put" $
          check abc1 $ runPrimBag abc0 $ putABC abc1 >> getABC
      it "can modify" $
          check abc2 $ runPrimBag abc0 $ modifyABC abcF >> getABC

newtype TypeA = TypeA Int
newtype TypeB = TypeB Int
newtype TypeC = TypeC Int

runPrimBag ::
    (Int, Int, Int) ->
    (forall s. Prim.StateBagT '[TypeA, TypeB, TypeC] (ST s) a) ->
    ((Int, Int, Int), a)
runPrimBag (a, b, c) m =
  runST $
  fmap (\(((ret, TypeA a), TypeB b), TypeC c) -> ((a, b, c), ret)) $
  Prim.runBagger $
  Prim.stackItem (TypeC c) $
  Prim.stackItem (TypeB b) $
  Prim.stackItem (TypeA a) $
  Prim.makeBag m

runPureBag ::
    (Int, Int, Int) ->
    Pure.StateBagT '[TypeA, TypeB, TypeC] Identity a ->
    ((Int, Int, Int), a)
runPureBag (a, b, c) m =
  runIdentity $
  fmap (\(((ret, TypeA a), TypeB b), TypeC c) -> ((a, b, c), ret)) $
  Pure.runBagger $
  Pure.stackItem (TypeC c) $
  Pure.stackItem (TypeB b) $
  Pure.stackItem (TypeA a) $
  Pure.makeBag m

getABC :: (StateBagMonad m, Bag m ~ '[TypeA, TypeB, TypeC]) =>
    m (Int, Int, Int)
getABC = do
    (TypeA a) <- getItem
    (TypeB b) <- getItem
    (TypeC c) <- getItem
    return (a, b, c)

putABC :: (StateBagMonad m, Bag m ~ '[TypeA, TypeB, TypeC]) =>
    (Int, Int, Int) -> m ()
putABC (a, b, c) = do
    putItem $ TypeA a
    putItem $ TypeB b
    putItem $ TypeC c

modifyABC :: (StateBagMonad m, Bag m ~ '[TypeA, TypeB, TypeC]) =>
    (Int -> Int, Int -> Int, Int -> Int) -> m ()
modifyABC (a, b, c) = do
    modifyItemM (\(TypeA x) -> return $ TypeA $ a x)
    modifyItemM (\(TypeB x) -> return $ TypeB $ b x)
    modifyItemM (\(TypeC x) -> return $ TypeC $ c x)
