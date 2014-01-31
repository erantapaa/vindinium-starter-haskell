{-# LANGUAGE NoMonomorphismRestriction #-}

module Util (
  TileGrid, DistanceGrid,
  isHero, isTavern, isMine, isHeroId, passable,
  toXY, toPos, offsetToXY, offsetToPos,
  findFeature, findMines, findTaverns, findHero, findHeroes,
  readBoard, dropUntil, chunksOf,
  showBoard,
  showIntGrid, formatGrid, interleave, justify,
  boardToGrid, atPos, inRangePos,
  distanceGrid, distances, adjacent, adjacent',
  bestMoves, bestMovesTo,
  -- vectorsToNearests,
  pathFromTo
) where

import Vindinium.Types (Tile(..), HeroId(..), Board(..), Pos(..), Dir(..))
import Data.Char (ord,chr)
import Data.List (dropWhile, isPrefixOf, splitAt, findIndex, findIndices, minimumBy)
import Data.Ord (comparing)
import Control.Monad (when, forM_)
import Data.Maybe (fromMaybe)
import qualified Data.Array.ST as ST
import Data.Array.IArray (IArray,elems,bounds,(!))
import Data.Array (Array, listArray)
import qualified Data.Array.Unboxed as U
import Data.Ix (inRange)
import Control.Monad.ST

type TileGrid = Array (Int,Int) Tile

type DistanceGrid = U.UArray (Int,Int) Int

-- | is functions

isHero :: Tile -> Bool
isHero (HeroTile _) = True
isHero _ = False

isTavern :: Tile -> Bool
isTavern TavernTile = True
isTavern _ = False

isMine :: Tile -> Bool
isMine (MineTile _) = True
isMine _ = False

isHeroId :: HeroId -> Tile -> Bool
isHeroId hid (HeroTile x) = x == hid
isHeroId _ _ = False

passable :: Tile -> Bool
passable WoodTile     = False
passable TavernTile   = False
passable (MineTile _) = False
passable FreeTile     = True
passable (HeroTile _) = True

tileIs :: (Tile -> Bool) -> TileGrid -> Pos -> Bool
tileIs pred tiles pos = pred $ tiles `atPos` pos

-- | pos functions

toXY :: Pos -> (Int,Int)
toXY pos = (posX pos, posY pos)

toPos :: (Int,Int) -> Pos
toPos (r,c) = Pos r c

offsetToXY :: Board -> Int -> (Int,Int)
offsetToXY board offset = (r,c)
  where (r,c) = offset `divMod` (boardSize board)

offsetToPos :: Board -> Int -> Pos
offsetToPos board offset = toPos $ offsetToXY board offset

-- | find functions

findFeature :: (Tile -> Bool) -> Board -> [ Pos ]
findFeature p board = map (offsetToPos board) $ findIndices p (boardTiles board)

findMines :: Board -> [ Pos ]
findMines = findFeature isMine

findHeroes :: Board -> [ Pos ]
findHeroes = findFeature isHero

findTaverns :: Board -> [ Pos ]
findTaverns = findFeature isTavern

findHero :: HeroId -> Board -> Maybe Pos
findHero hid board = do
  i <- findIndex (isHeroId hid) (boardTiles board)
  return $ offsetToPos board i

-- | board reading

readBoard :: String -> IO Board
readBoard path = do
  contents <- readFile path
  let lines' = tail $ dropUntil (isPrefixOf "+--") $ lines contents
      lines'' = takeWhile (isPrefixOf "|") lines'
      tiles = map parseTiles lines''
      rows = length tiles
      -- make sure all are of the same length
      badrows = filter (\(ts,k) -> length ts /= rows) $ zip tiles [1..]
  when (not $ null badrows) $ do
    forM_ badrows (\(ts,k) -> putStrLn $ "column mismatch on row " ++ show k)
    error "bad board"
  let board = Board rows (concat tiles)
  return board

parseTiles :: String -> [Tile]
parseTiles ('|':xs) = parseTiles xs
parseTiles ('#':_:xs) = WoodTile : parseTiles xs
parseTiles ('$':x:xs) = MineTile (toHero x) : parseTiles xs
parseTiles ('@':x:xs) = HeroTile (toHero' x) : parseTiles xs
parseTiles ('[':_:xs) = TavernTile : parseTiles xs
parseTiles (' ':_:xs) = FreeTile : parseTiles xs
parseTiles _ = []

toHero :: Char -> Maybe HeroId
toHero '-' = Nothing
toHero c
  | (c >= '1' && c <= '9')  = Just $ HeroId $ ord(c) - ord('0')
  | otherwise               = error $ "Invalid hero id: " ++ [c]

toHero' :: Char -> HeroId
toHero' c = fromMaybe (HeroId 1) (toHero c)

dropUntil :: (a -> Bool) -> [a] -> [a]
dropUntil p xs = dropWhile (not . p) xs

chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs =
  case splitAt n xs of
    ([],_)  -> []
    (as,bs) -> as : chunksOf n bs

-- | board display 

showBoard :: Board -> String
showBoard board = top ++ content ++ top
  where
    n = boardSize board
    top = "+" ++ (concat $ replicate n "--") ++ "+\n"
    rows = chunksOf n (boardTiles board)
    content = concatMap fmt rows
    fmt ts = "|" ++ (concatMap toChars ts) ++ "|\n"

idChar :: Int -> Char
idChar id
  | id >= 0 && id <= 9  = chr ( ord('0') + id )
  | otherwise           = '?'

toChars :: Tile -> String
toChars FreeTile = "  "
toChars WoodTile = "##"
toChars TavernTile = "[]"
toChars (HeroTile (HeroId id)) = "@" ++ [ idChar id ]
toChars (MineTile Nothing) = "$-"
toChars (MineTile (Just (HeroId id))) = "$" ++ [ idChar id ]

-- | grid display

formatGrid :: (IArray a e) => (e -> String) -> a (Int,Int) e -> String
formatGrid f arr =
  let  ((r0,c0),(r1,c1)) = bounds arr
       nthrow r = concat [ fmt $ arr ! (r,j) | j <- [c0..c1] ]
       fmt = justify (n+1) . f
       n = maximum $ map (length . f) (elems arr)
  in
  concat $ interleave (map nthrow [r0..r1]) (repeat "\n")

showIntGrid :: (Show e, Integral e, IArray a e) => a (Int,Int) e -> String
showIntGrid = formatGrid (\n -> if n >= 999 then "." else show n)

interleave :: [a] -> [a] -> [a]
interleave [] _ = []
interleave _ [] = []
interleave (x:xs) (y:ys) = x:y:interleave xs ys

justify :: Int -> String -> String
justify n s = spaces ++ s
  where spaces = replicate k ' '
        k = max 0 (n - length s)

-- | grid functions

addDir :: Pos -> Dir -> Pos
addDir (Pos r c) North = Pos (r-1) c
addDir (Pos r c) East  = Pos r     (c+1)
addDir (Pos r c) South = Pos (r+1) c
addDir (Pos r c) West  = Pos r     (c-1)

atPos :: (IArray a e) => a (Int,Int) e -> Pos -> e
atPos arr pos = arr ! (toXY pos)

inRangePos :: ((Int, Int), (Int, Int)) -> Pos -> Bool
inRangePos bnds pos = inRange bnds (toXY pos)

nsew = [(1,0),(-1,0),(0,1),(0,-1)]

boardToGrid :: Board -> TileGrid
boardToGrid board = listArray ((0,0),(n,n)) (boardTiles board)
  where n = (boardSize board)-1

adjacent' :: TileGrid -> Int -> Int -> [(Int,Int)]
adjacent' tiles r c = [ rc | (dx,dy) <- nsew, let rc = (r+dx,c+dy), inRange bnds rc, passable $ tiles ! rc ]
  where bnds = bounds tiles

adjacent :: TileGrid -> Pos -> [Pos]
adjacent tiles p = map toPos $ adjacent' tiles (posX p) (posY p)

-- | distance functions

distanceGrid :: TileGrid -> Pos -> DistanceGrid
distanceGrid = distances

distances :: TileGrid -> Pos -> DistanceGrid
distances tiles p = ST.runSTUArray $ distances' tiles (posX p) (posY p)

distance :: Pos -> DistanceGrid -> Int
distance pos dgrid = dgrid `atPos` pos

distances' :: TileGrid -> Int -> Int -> ST s (ST.STUArray s (Int, Int) Int)
distances' tiles r0 c0 = do
  arr <- ST.newArray (bounds tiles) 9999
  ST.writeArray arr (r0,c0) 0
  go arr r0 c0 1
  return arr
    where
      bnds = bounds tiles
      adj r c = [ rc | (dx,dy) <- nsew, let rc = (r+dx,c+dy), inRange bnds rc ]
      go arr r c z1 = do
        let adjs = adj r c
        forM_ adjs (\(r',c') -> do
          z' <- ST.readArray arr (r',c')
          when (z' > z1) $ do
            ST.writeArray arr (r',c') z1
            when (passable $ tiles ! (r',c')) $ go arr r' c' (z1+1)
          )

-- | vector functions

infs :: (Eq b, Ord b) => (a -> b) -> [a] -> [(b,a)]
infs _ [] = []
infs f xs = filter (\(b,a) -> b == smallest) pairs
  where
    pairs = zip (map f xs) xs
    smallest = fst $ minimumBy (comparing fst) pairs

infsBy :: Eq b => (a -> b) -> (b -> b -> Ordering) -> [a] -> [(b,a)]
infsBy _ _ [] = []
infsBy f comp xs = filter (\(b,a) -> b == smallest) pairs
  where pairs = zip (map f xs) xs
        smallest = fst $ minimumBy (\(b1,_) (b2,_) -> comp b1 b2) pairs

minimumsBy :: Ord b => (a -> b) -> [a] -> ([a],b)
minimumsBy f [] = error "minimumsBy empty list"
minimumsBy f (a:as) = go [a] (f a) as
  where go ms v []     = (ms,v)
        go ms v (a:as) = let v' = f a in
                         case compare v' v of
                           GT -> go ms v as
                           EQ -> go (a:ms) v as
                           LT -> go [a] v' as

nearests :: DistanceGrid -> [Pos] -> ([Pos], Int)
nearests _ []       = ([],undefined)
nearests dgrid ends = minimumsBy (dgrid `atPos`) ends

bestMoves :: TileGrid -> DistanceGrid -> Pos -> [Dir]
bestMoves tiles dgrid start = map snd bests
  where 
    moves = map (\dir -> (addDir start dir, dir)) [North, South, East, West]
    moves :: [ (Pos, Dir) ]
    moves' = filter (ok . fst) moves
    (bests,_) = minimumsBy (distTo.fst) moves'
    distTo :: Pos -> Int
    distTo p = dgrid `atPos` p
    ok :: Pos -> Bool
    ok p = inRangePos (bounds dgrid) p && ((tileIs passable tiles) p || distTo p == 0)

bestMovesTo :: TileGrid -> (Pos -> DistanceGrid) -> Pos -> Pos -> [Dir]
bestMovesTo tiles dgrid start end = bestMoves tiles (dgrid end) start

{-
vectorsToNearests :: (Pos -> DistanceGrid) -> Pos -> [ Pos ] -> [ ([Dir], Pos, Int) ]
vectorsToNearests distgrid start ends = 
  let (ps, d) = nearests (distgrid start) ends
  in [ (dirs, p, d) | p <- ps, let dirs = bestMoves (distgrid p) start ]
-}

pathFromTo :: TileGrid -> (Pos -> DistanceGrid) -> Pos -> Pos -> [Dir]
pathFromTo tiles dgridfn start end = go start
  where
    dgrid = dgridfn end
    go p0 = if p0 == end
              then []
              else case (bestMoves tiles dgrid p0) of
                     (m:_) -> m : go (p0 `addDir` m)
                     _     -> []

