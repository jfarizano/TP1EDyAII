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
search s@(k:ks) (Node c v l m r) | k == c = if ks == [] then v else search ks m
                                 | k < c = search s l
                                 | otherwise = search s r

-- Agrega un par (clave, valor) a un árbol. Si la clave ya está en el árbol,
-- actualiza su valor.
insert :: Ord k => [k] -> v -> TTree k v -> TTree k v
insert [k] v E = Leaf k v
insert (k:ks) v E = Node k Nothing E (insert ks v E) E
insert s@(k:ks) v (Leaf c b) | k == c = Node c (Just b) E (insert ks v E) E
                             | k < c = Node c (Just b) (insert s v E) E E
                             | otherwise = Node c (Just b) E E (insert s v E)
insert s@(k:ks) v (Node c b l m r) | k == c = if ks == [] then Node c (Just v) l m r
                                                          else Node c b l (insert ks v m) r
                                   | k < c = Node c b (insert s v l) m r
                                   | otherwise = Node c b l m (insert s v r)

-- Elimina una clave y el valor asociada a ésta en un árbol
-- Limpiando nodos huérfanos, y convirtiendo en hoja los nodos sin hijos.
delete :: Ord k => [k] -> TTree k v -> TTree k v
delete _ E = E
delete [] n = n
delete [k] n@(Leaf c v) = if k == c then E else n
delete (_:_) n@(Leaf c v) = n
delete s@(k:ks) n@(Node c v l E E) | k == c = if ks == [] then l 
                                                          else n
                                   | k < c = reorder (Node c v (delete s l) E E)
                                   | otherwise = n
delete s@(k:ks) n@(Node c v E E r) | k == c = if ks == [] then r 
                                                          else n
                                   | k > c = reorder (Node c v E E (delete s r))
                                   | otherwise = n
delete (k:ks) n@(Node c v E m E) | k == c = if ks == [] then Node c Nothing E m E
                                                        else reorder (Node c v E (delete ks m) E)
                                 | otherwise = n
delete s@(k:ks) n@(Node c v l E r) | k == c = if ks == [] then let (nk, nv, nm) = maximumT l
                                                               in Node nk nv (delMax l) nm r
                                                          else n
                                   | k < c = Node c v (delete s l) E r
                                   | otherwise = Node c v l E (delete s r)
delete s@(k:ks) n@(Node c v l m E) | k == c = if ks == [] then Node c Nothing l m E
                                                          else reorder (Node c v l (delete ks m) E)
                                   | k < c = Node c v (delete s l) m E
                                   | otherwise = n
delete s@(k:ks) n@(Node c v E m r) | k == c = if ks == [] then Node c Nothing E m r
                                                          else reorder (Node c v E (delete ks m) r)
                                   | k > c = Node c v E m (delete s r)
                                   | otherwise = n                                                                     
delete s@(k:ks) n@(Node c v l m r) | k == c = if ks == [] then Node c Nothing l m r
                                                          else Node c v l (delete ks m) r
                                   | k < c = Node c v (delete s l) m r
                                   | otherwise = Node c v l m (delete s r)

-- Dado un nodo, si este no tiene hijos y tiene valor Nothing, devuelve vacío
-- si guarda un valor Just a, devuelve una hoja con la misma clave y el valor a.
-- Si tiene un solo hijo lateral, devuelve el mismo reordenado.
-- En caso contrario (tiene hijos), devuelve el mismo nodo
reorder :: Ord k => TTree k v -> TTree k v
reorder (Node _ Nothing E E E) = E
reorder (Node _ Nothing l E E) = reorder l
reorder (Node _ Nothing E E r) = reorder r
reorder (Node c v E E E) = (Leaf c (fromJust v))
reorder n = n

-- Busca el nodo con la clave máxima en un árbol, devuelve su clave, valor
-- y su hijo del medio en una 3-upla
maximumT :: Ord k => TTree k v -> (k, Maybe v, TTree k v)
maximumT (Leaf k v) = (k, Just v, E)
maximumT (Node k v _ m E) = (k, v, m)
maximumT (Node _ _ _ _ r) = maximumT r

-- Elimina el hijo máximo de un árbol, si este tenía un hijo izquierdo lo
-- devuelve, y si era una hoja devuelve vacío
delMax :: Ord k => TTree k v -> TTree k v
delMax (Leaf _ _) = E
delMax (Node k v l m E) = l
delMax (Node k v l m r) = reorder (Node k v l m (delMax r))

-- Dado un  ́arbol devuelve una lista ordenada con las claves del mismo
keys :: Ord k => TTree k v -> [[k]]
keys = keys' []

keys' :: Ord k => [k] -> TTree k v -> [[k]]
keys' _ E = []
keys' c (Leaf k v) = [c ++ [k]] 
keys' c (Node k v l m r) = case v of
                                Nothing -> (keys' c l) ++ (keys' (c ++ [k]) m) ++ (keys' c r)
                                _       -> (keys' c l) ++ [c ++ [k]] ++ (keys' (c ++ [k]) m) ++ (keys' c r)                         

class Dic k v d | d -> k v where 
  vacio :: d
  insertar :: Ord k => k -> v -> d -> d
  buscar :: Ord k => k -> d -> Maybe v
  eliminar :: Ord k => k -> d -> d
  claves :: Ord k => d -> [k]

instance Ord k => Dic [k] v (TTree k v) where
  vacio = E
  insertar = insert
  buscar = search
  eliminar = delete
  claves = keys

t = insert "se" 8 (insert "sin" 7 (insert "si" 4 (insert "ras" 1 (insert "res" 4 (insert "red" 9 (insert "reo" 2 (insert "re" 16 E)))))))

q = insert "ree" 1 (insert "refi" 6 (insert "ref" 3 (insert "rea" 5 (insert "redes" 6 t))))

e = insert "ref" 4 t

y = delete "si" (delete "se" (insert "sz" 5 t))