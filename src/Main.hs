{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Options.Applicative

import Vindinium
import Bot

import Data.String (fromString)
import Data.Text (pack, unpack)
import System.IO (hSetBuffering, stdout, BufferMode(..))

data Cmd = Training Settings (Maybe Int) (Maybe BoardId)
         | Arena Settings
         deriving (Show, Eq)

cmdSettings :: Cmd -> Settings
cmdSettings (Training s _ _) = s
cmdSettings (Arena s) = s

settings :: Parser Settings
settings = Settings <$> (Key <$> argument (Just . pack) (metavar "KEY"))
                    <*> (fromString <$> strOption (long "url" <> value "http://vindinium.org"))

trainingCmd :: Parser Cmd
trainingCmd = Training <$> settings
                       <*> optional (option (long "turns"))
                       <*> optional (strOption (long "map"))

arenaCmd :: Parser Cmd
arenaCmd = Arena <$> settings

cmd :: Parser Cmd
cmd = subparser
    ( command "training" (info trainingCmd
        ( progDesc "Run bot in training mode" ))
   <> command "arena" (info arenaCmd
        (progDesc "Run bot in arena mode" ))
    )

runCmd :: Cmd -> IO ()
runCmd c  = do
    s <- runVindinium (cmdSettings c) $ do
        case c of
            (Training _ t b) -> playTraining t b bot
            (Arena _)        -> playArena bot

    putStrLn $ "Game finished: " ++ unpack (stateViewUrl s)

initIO :: IO ()
initIO = hSetBuffering stdout LineBuffering

main :: IO ()
main = do
    initIO
    execParser opts >>= runCmd
  where
    opts = info (cmd <**> helper) idm
