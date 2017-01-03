{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, GeneralizedNewtypeDeriving #-}
module Environment ( Env(..)
                 , empty
                 , lookup
                 , add
                 , addAll
                 , update
                 , union
                 , keys
                 ) where

import qualified Data.Map as M
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import Data.Monoid
import Prelude hiding (lookup)

newtype Env e = Env (M.Map String e) deriving (Functor, Foldable, Traversable, Read, Show, Eq, Ord, Monoid)

empty :: Env e
empty = Env M.empty

lookup :: Env e -> String -> Maybe e
lookup (Env env) var = M.lookup var env

add :: Env e -> (String, e) -> Env e
add (Env env) (key, elt) = Env (M.insert key elt env)

addAll :: Env e -> [(String, e)] -> Env e
addAll (Env env) pairs = Env $ foldr (\(k,e) g -> M.insert k e g) env pairs

update :: (e -> Maybe e) -> String -> Env e -> Env e 
update f key (Env g) = Env $ M.update f key g 

union :: Env e -> Env e -> Env e
union (Env g) (Env g') = Env $ M.union g g'

map :: (e -> e) -> Env e -> Env e 
map f (Env g) = Env $ M.map f g

keys :: Env e -> [String]
keys (Env e) = M.keys e