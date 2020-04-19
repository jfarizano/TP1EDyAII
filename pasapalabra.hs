{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

import Data.Maybe

data TTree k v = Node k (Maybe v) (TTree k v) (TTree k v) (TTree k v) | 
                 Leaf k v | 
                 E deriving Show

-- Devuelve el valor asociado a una clave.
search :: Ord k => [k] -> TTree k v -> Maybe v
search _ E = Nothing
search [] _ = Nothing
search [k] (Leaf c v) = if k == c then Just v else Nothing
search (_:_) (Leaf c v) = Nothing
search (k:ks) (Node c v l m r) | k == c = if length (k:ks) == 1 then v else search ks m
                               | k < c = search (k:ks) l
                               | otherwise = search (k:ks) r

-- Agrega un par (clave, valor) a un árbol. Si la clave ya está en el árbol,
-- actualiza su valor.
insert :: Ord k => [k] -> v -> TTree k v -> TTree k v
insert [k] v E = Leaf k v
insert (k:ks) v E = Node k Nothing E (insert ks v E) E
insert [k] v (Leaf c b) | k == c = Leaf k v
                        | k < c = Node c (Just b) (Leaf k v) E E
                        | k > c = Node c (Just b) E E (Leaf k v)
insert (k:ks) v (Leaf c b) | k == c = Node c (Just b) E (insert ks v E) E
                           | k < c = Node c (Just b) (insert (k:ks) v E) E E
                           | k > c = Node c (Just b) E E (insert (k:ks) v E)
insert [k] v (Node c b l m r) | k == c = Node c (Just v) l m r
                              | k < c = Node c b (insert [k] v l) m r
                              | k > c = Node c b l m (insert [k] v r)
insert (k:ks) v (Node c b l m r) | k == c = Node c b l (insert ks v m) r
                                 | k < c = Node c b (insert (k:ks) v l) m r
                                 | k > c = Node c b l m (insert (k:ks) v r)

-- Elimina una clave y el valor asociada a ésta en un árbol
-- delete :: Ord k => [k] -> TTree k v -> TTree k v
-- delete _ E = E 
-- delete [k] n@(Leaf c v) = if k == c then E else n
-- delete (k:ks) n@(Leaf c v) = n
-- delete (k:ks) v n@(Node c b l m r) = undefined

-- Dado un  ́arbol devuelve una lista ordenada con las claves del mismo
keys :: Ord k => TTree k v -> [[k]]
keys E = []
keys (Leaf k v) = [[k]]
keys (Node k v l m r) = (keys' [] l) ++ (keys' [k] m) ++ (keys' [] r)

keys' :: Ord k => [k] -> TTree k v -> [[k]]
keys' _ E = []
keys' c (Leaf k v) = [c ++ [k]] 
keys' c (Node k v l m r) = case v of
                                Nothing -> (keys' c l) ++ (keys' (c ++ [k]) m) ++ (keys' c r)
                                _       -> (keys' c l) ++ [c ++ [k]] ++ (keys' (c ++ [k]) m) ++ (keys' c r)                         

class Dic k v d | d -> k v where 
  vacio :: d
  insertar :: Ord k => [k] -> v -> d -> d
  buscar :: Ord k => [k] -> d -> Maybe v
  eliminar :: Ord k => [k] -> d -> d
  claves :: Ord k => d -> [[k]]

instance Dic k v (TTree k v) where
  vacio = E
  insertar k v t = insert k v t
  buscar k t = search k t
  eliminar k t = undefined
  claves t = keys t

simple = Node 'r' Nothing E (Node 'e' (Just 16) (Node 'a' (Just 20) E E E) E E) E

t = insert "se" 8 (insert "sin" 7 (insert "si" 4 (insert "ras" 1 (insert "res" 4 (insert "red" 9 (insert "reo" 2 (insert "re" 16 E)))))))