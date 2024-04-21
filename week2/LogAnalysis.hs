{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

-- Format of message lines:
--
-- (I|W) <timestamp> <content>
-- E <severity> <timestamp> <content>

parseMessage :: String -> LogMessage
parseMessage line = case tokens of
  ("I" : ts : msg) -> LogMessage Info (read ts) (unwords msg)
  ("W" : ts : msg) -> LogMessage Warning (read ts) (unwords msg)
  ("E" : s : ts : msg) -> LogMessage (Error (read s)) (read ts) (unwords msg)
  _ -> Unknown line
  where
    tokens = words line

parse :: String -> [LogMessage]
parse = map (parseMessage) . lines

timeStamp :: LogMessage -> Int
timeStamp logMessage = case logMessage of
  (LogMessage (Error _) ts _) -> ts
  (LogMessage _ ts _) -> ts
  _ -> -1

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert msg (Node l m r)
  | timeStamp msg < timeStamp m = Node (insert msg l) m r
  | otherwise = Node l m (insert msg r)

build :: [LogMessage] -> MessageTree
build = foldl (\tree msg -> insert msg tree) Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l m r) = inOrder l ++ [m] ++ inOrder r

getLogContent :: LogMessage -> String
getLogContent (Unknown message) = message
getLogContent (LogMessage _ _ message) = message

sed :: (LogMessage -> Bool) -> String -> [LogMessage]
sed f content = filter f $ parse content

sort :: [LogMessage] -> [LogMessage]
sort = inOrder . build

isError :: LogMessage -> Bool
isError (LogMessage (Error _) _ _) = True
isError _ = False

severity :: LogMessage -> Int
severity (LogMessage (Error s) _ _) = s
severity _ = -1

isWarn :: LogMessage -> Bool
isWarn (LogMessage Warning _ _) = True
isWarn _ = False

isInfo :: LogMessage -> Bool
isInfo (LogMessage Info _ _) = True
isInfo _ = False

isUnknown :: LogMessage -> Bool
isUnknown (Unknown _) = True
isUnknown _ = False

filterFile inFile f outFile = do
  txt <- readFile inFile
  let txt' = foldl (\x y -> x ++ "\n" ++ y) "" $ f $ parse txt
  writeFile outFile txt'

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong =
  map getLogContent
    . take 50
    . inOrder
    . build
    . filter filterMsg

filterMsg :: LogMessage -> Bool
filterMsg (LogMessage (Error s) _ _) = s >= 50
filterMsg _ = False
