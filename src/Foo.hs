
import Vindinium.Types (Tile(..), HeroId(..), Board(..))
import Data.Char (ord,chr)
import Data.List (dropWhile, isPrefixOf, splitAt, findIndex)
import Control.Monad (when, forM_)
import Data.Maybe (fromMaybe)
import qualified Data.Array.ST as ST
import Data.Array (Array, listArray, (!), bounds, elems)
import Data.Ix (inRange)
import Control.Monad.ST

type TileGrid = Array (Int,Int) Tile

isMine :: Tile -> Bool
isMine (MineTile _) = True
isMine _ = False

numMines :: Board -> Int
numMines b = length $ filter isMine (boardTiles b)

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
  | (c >= '1' && c <= '4')  = Just $ HeroId $ ord(c) - ord('0')
  | otherwise               = error $ "Invalid hero id: " ++ [c]

toHero' :: Char -> HeroId
toHero' c = fromMaybe (HeroId 1) (toHero c)

dropUntil :: (a -> Bool) -> [a] -> [a]
dropUntil p xs = dropWhile (not . p) xs

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

chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs = 
  case splitAt n xs of
    ([],_)  -> []
    (as,bs) -> as : chunksOf n bs

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

showBoard :: Board -> String
showBoard board = top ++ content ++ top
  where 
    n = boardSize board
    top = "+" ++ (concat $ replicate n "--") ++ "+\n"
    rows = chunksOf n (boardTiles board)
    content = concatMap fmt rows
    fmt ts = "|" ++ (concatMap toChars ts) ++ "|\n"

boardToArray :: Board -> TileGrid
boardToArray board = listArray ((1,1),(n,n)) (boardTiles board)
  where n = boardSize board

passable :: Tile -> Bool
passable WoodTile     = False
passable TavernTile   = False
passable (MineTile _) = False
passable FreeTile     = True
passable (HeroTile _) = True

nsew = [(1,0),(-1,0),(0,1),(0,-1)]

adjacent :: TileGrid -> Int -> Int -> [(Int,Int)]
adjacent tiles r c = [ rc | (dx,dy) <- nsew, let rc = (r+dx,c+dy), inRange bnds rc, passable $ tiles ! rc ]
  where bnds = bounds tiles

distances' :: TileGrid -> Int -> Int -> ST s (ST.STArray s (Int, Int) Int)
distances' tiles r0 c0 = do
  arr <- ST.newArray (bounds tiles) 9999 
  ST.writeArray arr (r0,c0) 0
  go arr r0 c0 1
  return arr
    where
      go arr r c z1 = do
        let adjs = adjacent tiles r c
        forM_ adjs (\(r',c') -> do
          z' <- ST.readArray arr (r',c')
          when (z' > z1) $ do
            ST.writeArray arr (r',c') z1
            go arr r' c' (z1+1)
          )

-- compute distances from (r,c) to all other points on the grid
distances :: TileGrid -> Int -> Int -> Array (Int,Int) Int
distances tiles r c = ST.runSTArray $ distances' tiles r c

interleave :: [a] -> [a] -> [a]
interleave [] _ = []
interleave _ [] = []
interleave (x:xs) (y:ys) = x:y:interleave xs ys

formatArray :: (e -> String) -> Array (Int,Int) e -> String
formatArray f arr =
  let  ((r0,c0),(r1,c1)) = bounds arr
       nthrow r = concat [ fmt $ arr ! (r,j) | j <- [c0..c1] ]
       fmt = justify (n+1) . f
       n = maximum $ map (length . f) (elems arr)
  in
  concat $ interleave (map nthrow [r0..r1]) (repeat "\n")

justify :: Int -> String -> String
justify n s = spaces ++ s
  where spaces = replicate k ' '
        k = max 0 (n - length s)

showIntMap'' = formatArray (\n -> if n >= 999 then "." else show n)

offsetToXY :: Board -> Int -> (Int,Int)
offsetToXY board offset = (r+1,c+1)
  where (r,c) = offset `divMod` (boardSize board)

isHero :: HeroId -> Tile -> Bool
isHero hid (HeroTile x) = x == hid
isHero _ _ = False

findHero :: HeroId -> Board -> Maybe (Int,Int)
findHero hid board = do
  i <- findIndex (isHero hid) (boardTiles board)
  return $ offsetToXY board i

test1 = do b <- readBoard "board"
           let mrc = findHero (HeroId 1) b
           putStrLn $ "location of hero 1: " ++ show mrc

test2 = do b <- readBoard "board"
           let Just (r,c) = findHero (HeroId 1) b
               d = distances (boardToArray b) r c
           putStrLn $ "Hero 1 is at " ++ show (r,c)
           putStr $ showIntMap'' d

main = test2

