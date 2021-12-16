{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Graph (module Data.Graph) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe)
import Control.Monad (guard)
import qualified Data.Sequence as Seq
import qualified Data.Heap                     as H

type Distances a = M.Map (Vertex a) Int
type Prevs a = M.Map (Vertex a) (Vertex a)

newtype Vertex a = Keyed { unVertex :: a }
  deriving (Ord, Eq, Show)

class Vertices g where
  vertices :: Ord a => g a -> S.Set (Vertex a)

class Vertices g => Graph g where
  neighbors :: Ord a => g a -> Vertex a -> S.Set (Int, Vertex a)

  removeEdge :: Ord a => g a -> (Vertex a, Vertex a) -> g a

class Vertices g => EquallyWeightedGraph g where
  equalNeighbors :: Ord a => g a -> Vertex a -> S.Set (Vertex a)

newtype MapGraph a = Weighted { toMap :: M.Map (Vertex a) (S.Set (Int, Vertex a)) }

newtype EqualMapGraph a = EquallyWeighted' { toMap' :: M.Map (Vertex a) (S.Set (Vertex a)) }

instance Graph MapGraph where
  neighbors (Weighted m) v = M.findWithDefault S.empty v m

  removeEdge (Weighted m) (src, dest) = Weighted $ M.update (remove dest) src m

   where
    remove v set =
      let set' = S.filter ((/=) v . snd) set
      in  if S.null set' then Nothing else Just set'

instance Vertices MapGraph where
  vertices (Weighted m) =
    S.union (M.keysSet m) $ S.unions $ S.map snd . snd <$> M.toList m

newtype ListGraph a = EquallyWeighted { toList :: [(Vertex a, Vertex a)] }

instance Graph EqualMapGraph where
  neighbors (EquallyWeighted' m) v =
    S.map (1, ) $ M.findWithDefault S.empty v m

  removeEdge (EquallyWeighted' m) (src, dest) = EquallyWeighted'
    $ M.update (remove dest) src m

   where
    remove v set =
      let set' = S.filter (/= v) set
      in  if S.null set' then Nothing else Just set'

instance EquallyWeightedGraph EqualMapGraph where
  equalNeighbors (EquallyWeighted' m) v = M.findWithDefault S.empty v m

instance Vertices EqualMapGraph where
  vertices (EquallyWeighted' m) =
    S.union (M.keysSet m) $ S.unions $ map snd $ M.toList m

instance EquallyWeightedGraph ListGraph where
  equalNeighbors (EquallyWeighted vs) v =
    S.fromList $ map snd $ filter ((==) v . fst) vs

listGraphToMapGraph :: Ord a => ListGraph a -> EqualMapGraph a
listGraphToMapGraph (EquallyWeighted vs) = EquallyWeighted'
  $ foldr insert M.empty vs
  where insert (a, b) = M.alter (Just . S.insert b . fromMaybe S.empty) a

listGraphToMapGraph' :: Ord a => ListGraph a -> MapGraph a
listGraphToMapGraph' (EquallyWeighted vs) = Weighted $ foldr insert M.empty vs
  where insert (a, b) = M.alter (Just . S.insert (1, b) . fromMaybe S.empty) a

instance Vertices ListGraph where
  vertices (EquallyWeighted vs) =
    S.union (S.fromList $ map fst vs) (S.fromList $ map snd vs)

-- | Search for all shortest paths in a graph using Dijkstra's algorithm
--
-- Returns shortest distances from the given 'Vertex' as well as the breadcrumb
-- path to get there.
--
-- Examples:
--
-- >>> :{
--  let
--    aToBDAndC = (Keyed 'a', S.fromList [(1, Keyed 'b'), (3, Keyed 'c'), (3, Keyed 'd')])
--    bToD      = (Keyed 'b', S.fromList [(1, Keyed 'd')])
--    cToD      = (Keyed 'c', S.fromList [(1, Keyed 'd')])
--    graph     = Weighted $ M.fromList [aToBDAndC, bToD, cToD]
--    y = 2
--  in M.lookup (Keyed 'd') $ fst $ dijkstra graph $ Keyed 'a'
-- :}
-- Just 2
--
-- >>> :{
--  let
--    aToBDAndC = (Keyed 'a', S.fromList [(1, Keyed 'b'), (3, Keyed 'c'), (3, Keyed 'd')])
--    bToD      = (Keyed 'b', S.fromList [(1, Keyed 'd')])
--    cToD      = (Keyed 'c', S.fromList [(1, Keyed 'd')])
--    graph     = Weighted $ M.fromList [aToBDAndC, bToD, cToD]
--    y = 2
--  in fmap unVertex $ M.lookup (Keyed 'd') $ snd $ dijkstra graph $ Keyed 'a'
-- :}
-- Just 'b'
--
dijkstra
  :: forall g a . (Graph g, Ord a) => g a -> Vertex a -> (Distances a, Prevs a)
dijkstra graph source = aux initialQ initialDist M.empty

 where
  initialDist = M.singleton source 0
  initialQ :: H.MinPrioHeap Int (Vertex a)
  initialQ = H.fromList $ fmap qRecord $ S.toList $ vertices graph

  qRecord v = (initialScore v, v)

  initialScore v = if v == source then 0 else maxBound

  uncurry3 f (x, y, z) = f x y z

  aux q dist prev = fromMaybe (dist, prev) $ do
    ((_, u'), q') <- H.view q
    pure $ uncurry3 aux $ foldr (insertShorter u') (q', dist, prev) $ neighbors
      graph
      u'

  insertShorter u (len, v) (q, dist, prev) = fromMaybe (q, dist, prev) $ do
    uD <- M.lookup u dist
    let alt = uD + len
    guard $ alt < M.findWithDefault maxBound v dist
    pure (H.insert (alt, v) q, M.insert v alt dist, M.insert v u prev)

class Steps a v where
  step :: Vertex v -> a -> a
  start :: Vertex v -> a

instance Steps Int b where
  step = const (+ 1)
  start _ = 0

instance Steps (Seq.Seq (Vertex a)) a where
  step v path = path Seq.|> v
  start = const Seq.empty

instance (Steps a c, Steps b c) => Steps (a, b) c where
  step v (x, y) = (step v x, step v y)
  start v = (start v, start v)

instance Steps () a where
  step _ _ = ()
  start _ = ()

walk
  :: (EquallyWeightedGraph g, Ord a, Steps r a, Ord r)
  => g a
  -> Vertex a
  -> (Vertex a -> Bool)
  -> (Maybe r, S.Set (Vertex a))
walk graph source end = search (S.singleton source)
                               (Seq.singleton (source, start source))

 where
  search visited toTry = case Seq.viewl toTry of
    Seq.EmptyL -> (Nothing, visited)
    (at, d) Seq.:< rest ->
      let
        neighbors' = equalNeighbors graph at S.\\ visited
        visited'   = S.union neighbors' visited
        moreToTry =
          Seq.fromList $ S.toList $ S.map (\n -> (n, step n d)) neighbors'
      in
        if end at
          then (Just d, visited)
          else search visited' $ rest Seq.>< moreToTry

bfs
  :: (EquallyWeightedGraph g, Ord a, Steps r a, Ord r)
  => g a
  -> Vertex a
  -> Vertex a
  -> Maybe r
bfs graph source sought = fst $ walk graph source (sought ==)

subGraph
  :: (EquallyWeightedGraph g, Ord a) => g a -> Vertex a -> S.Set (Vertex a)
subGraph graph source = case walk graph source $ const False of
  (Just (), visited) -> visited
  (Nothing, visited) -> visited

topologicalSort :: (Graph g, Ord a) => g a -> [a]
topologicalSort graph = go nodesWithoutIncomingEdge graph
 where

  go s graph' = case S.minView s of
    Nothing -> []
    Just (n, s') ->
      let
        ms      = neighbors graph' n
        graph'' = foldr (\(_, m) g -> removeEdge g (n, m)) graph' ms
        additionalSs =
          S.map snd $ S.filter (not . hasIncomingEdges graph'' . snd) ms
      in
        unVertex n : go (S.union additionalSs s') graph''

  hasIncomingEdges g v =
    any (S.member v . S.map snd . neighbors g) $ vertices g

  nodesWithoutIncomingEdge =
    let vs = vertices graph
    in  vs S.\\ S.unions (S.toList $ S.map (S.map snd . neighbors graph) vs)
