{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

data TTree k v = Node k (Maybe v) (TTree k v) (TTree k v) (TTree k v) | 
                 Leaf k v | 
                 E

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
insert = undefined

-- Elimina una clave y el valor asociada a ésta en un árbol
delete :: Ord k => [k] -> TTree k v -> TTree k v 
delete = undefined

-- Dado un  ́arbol devuelve una lista ordenada con las claves del mismo
keys :: Ord k => TTree k v -> [[k]]
keys = undefined 

class Dic k v d | d -> k v where 
  vacio :: d 
  insertar :: Ord k => k -> v -> d -> d
  buscar :: Ord k => k -> d -> Maybe v
  eliminar :: Ord k => k -> d -> d
  claves :: Ord k => d -> [(k,v)]

t = Node 'r' Nothing E 
                    (Node 'e' (Just 16) (Node 'a' Nothing E 
                                                         (Leaf 's' 1) 
                                                         E)
                                        (Node 'o' (Just 2) (Leaf 'd' 9) 
                                                           E 
                                                           (Leaf 's' 4)) 
                                        E)
                    (Node 's' Nothing E 
                                      (Node 'i' (Just 4) (Leaf 'e' 8) 
                                                         (Leaf 'n' 7) 
                                                         E) 
                                      E)

simple = Node 'r' Nothing E (Node 'e' (Just 16) (Node 'a' (Just 20) E E E) E E) E