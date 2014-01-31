
import Vindinium.Types
import Util
import Control.Monad (forM_)

test2 = do b <- readBoard "board"
           let Just pos = findHero (HeroId 1) b
               d = distances (boardToGrid b) pos
           putStrLn $ "Hero 1 is at " ++ show pos
           putStr $ showIntGrid d

test3 = do b <- readBoard "board"
           let Just pos = findHero (HeroId 1) b
               d = distances (boardToGrid b) pos
           putStrLn $ "Hero 1 is at " ++ show pos
           let taverns = findTaverns b
           putStrLn $ "Taverns are at: " ++ show taverns

test4 = do b <- readBoard "board"
           let Just pos = findHero (HeroId 1) b
               tiles = boardToGrid b
               d = distances tiles pos
           putStrLn $ "Hero 1 is at " ++ show pos
           let taverns = findTaverns b
           putStrLn $ "Taverns are at: " ++ show taverns
           let tiles = boardToGrid b
               dgrid :: Pos -> DistanceGrid
               dgrid = distanceGrid tiles
           forM_ taverns $ \t -> do
             let moves = bestMovesTo dgrid pos t
             putStrLn $ "  to tavern at " ++ show t ++ ": " ++ show moves

test5 = do b <- readBoard "board"
           let Just pos = findHero (HeroId 1) b
               tiles = boardToGrid b
               d = distances tiles pos
           putStrLn $ "Hero 1 is at " ++ show pos
           let taverns = findTaverns b
           putStrLn $ "Taverns are at: " ++ show taverns
           let tiles = boardToGrid b
               dgrid :: Pos -> DistanceGrid
               dgrid = distanceGrid tiles
           forM_ taverns $ \t -> do
             let path = pathFromTo dgrid pos t
             putStrLn $ "  to tavern at " ++ show t ++ ": " ++ show path

