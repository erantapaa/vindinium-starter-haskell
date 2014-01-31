module Bot
        ( bot
        )
    where

import Vindinium

import System.Random (getStdRandom, randomR)
import Data.Maybe (fromJust)
import Control.Monad (liftM, when, forM_)
import Control.Monad.IO.Class (liftIO)
import Util (TileGrid, DistanceGrid, boardToGrid, isTavern, findFeature, distanceGrid, atPos, showBoard, bestMoves, isMine, offsetToPos,showIntGrid)
import Data.List (findIndices,sortBy)
import Data.Ord (comparing)
import Debug.Trace (trace)

debug :: Show a => String -> a -> a
debug prefix a = trace ("=== " ++ prefix ++ ": " ++ show a ++ "\n") a

bot :: Bot
bot = bot2

-- move to the nearest un-owned mine or tavern
bot2 :: Bot
bot2 state = do
  liftIO $ do
    when (turn == 0) printBoard
    putStrLn $ "\n=== Turn " ++ show turn ++ " ===\n"
    reportState state
    emitMineReport board
  (targets,kind) <- liftIO $ do
    if life < 40
      then do putStrLn "=== finding a tavern"
              let taverns = findFeature isTavern board
              return (taverns,"taverns")
      else do putStrLn "=== finding a mine"
              let mines = findFeature (notOwnedMine myId) board
              return (mines,"mines")
  let myDgrid = distanceGrid tiles myPos
  let distToMe :: Pos -> Int
      distToMe p = myDgrid `atPos` p
      sorted :: [Pos]
      sorted = sortBy (comparing distToMe) targets
      foo = map (\p -> (p, distToMe p)) sorted
  liftIO $ putStrLn $ "=== " ++ kind ++ " sorted by distance: " ++ show foo
  let target = if null sorted then Pos 0 0 else head sorted
  let dgrid = distanceGrid tiles target
  let dirs = debug "dirs" $ bestMoves tiles dgrid myPos
  liftIO $ putStrLn $ "Best move(s) to " ++ show target ++ ": " ++ show dirs
  return $ head dirs
  where
    game = stateGame state
    me = stateHero state
    myId = heroId me
    myPos = heroPos me
    life = heroLife me
    turn = gameTurn game
    board = gameBoard game
    tiles = boardToGrid board
    printBoard = putStrLn $ showBoard board

-- utility functions

emitMineReport :: Board -> IO ()
emitMineReport board = do
  let offsets = findIndices isMine (boardTiles board)
  putStrLn "=== Mine Report ==="
  forM_ offsets $ \offset -> do
    let pos = offsetToPos board offset
        tile = (boardTiles board) !! offset
    putStrLn $ " - mine at " ++ show pos ++ " - " ++ (show tile)

compareHeros :: TileGrid -> [Hero] -> IO ()
compareHeros tiles hs =
  forM_ hs $ \h -> do
    let pos = heroPos h
    putStrLn $ " - hero at " ++ show pos ++ " tile: " ++ show (tiles `atPos` pos)

compareMines :: TileGrid -> Board -> IO ()
compareMines tiles board = compareOffsets tiles board offsets
  where offsets = findIndices isMine (boardTiles board)

compareOffsets :: TileGrid -> Board -> [Int] -> IO ()
compareOffsets tiles board offsets =
  forM_ offsets $ \offset -> do
    let pos = offsetToPos board offset
    putStrLn $ " - tile at offset " ++ show offset ++ " pos: " ++ show pos ++ " tile: " ++ show (tiles `atPos` pos)

tileGrid :: State -> TileGrid
tileGrid state = boardToGrid (gameBoard $ stateGame state)

{-
moveToClosest :: (Tile -> Bool) -> State -> Dir
moveToClosest pred state =
  case vecs of
    ( ((dir:_),_,_) : _) -> dir
    _                    -> Stay
  where game = stateGame state
        me = stateHero state
        myPos = heroPos me
        board = gameBoard game
        tiles = boardToGrid board
        targets = debug "targets" $ findFeature pred board
        dgrid :: Pos -> DistanceGrid
        dgrid p = distanceGrid tiles p
        vecs = vectorsToNearests dgrid myPos targets

moveToMine :: HeroId -> State -> Dir
moveToMine id = moveToClosest (notOwnedMine id)

moveToTavern :: State -> Dir
moveToTavern = moveToClosest isTavern
-}

notOwnedMine :: HeroId -> Tile -> Bool
notOwnedMine _  (MineTile Nothing)  = True
notOwnedMine id (MineTile (Just x)) = x /= id
notOwnedMine _  _                   = False

reportState :: State -> IO ()
reportState state =
  putStrLn $ "Turn: " ++ show turn ++ " Location: " ++ show pos ++ " Life: " ++ show life ++ " Gold: " ++ show gold ++ " Mines: " ++ show mines
  where game = stateGame state
        turn = gameTurn game
        me = stateHero state
        pos = heroPos me
        life = heroLife me
        gold = heroGold me
        mines = heroMineCount me

randomBot :: Bot
randomBot _ = liftM fromJust $ liftIO $ pickRandom [Stay, North, South, East, West]

inBoard :: Board -> Pos -> Bool
inBoard b (Pos x y) =
    let s = boardSize b
    in x >= 0 && x < s && y >= 0 && y < s

tileAt :: Board -> Pos -> Maybe Tile
tileAt b p@(Pos x y) =
    if inBoard b p
        then Just $ boardTiles b !! idx
        else Nothing
  where
    idx = y * boardSize b + x

pickRandom :: [a] -> IO (Maybe a)
pickRandom [] = return Nothing
pickRandom xs = do
    idx <- getStdRandom (randomR (0, length xs - 1))
    return . Just $ xs !! idx
