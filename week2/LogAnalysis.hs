{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

-- Format of message lines:
--
-- (I|W) <timestamp> <content>
-- E <severity> <timestamp> <content>


-- | Get the timestamp out of a log message.
timeStamp :: LogMessage -> Int
timeStamp msg = case msg of
  (LogMessage _ ts _) -> ts
  _ -> -1

logMsg :: LogMessage -> String
logMsg (Unknown message) = message
logMsg (LogMessage _ _ message) = message

{- Exercise 1 -}

-- | Parse and individual line from the log file.
--
-- /Note:/ This function assumes that lines starting with "I", "W", or "E" are
-- in the right format. I.e. "I am not an int" will result in an exception.
-- Everything else is considered @Unknown@:
parseMessage :: String -> LogMessage
parseMessage line = case tokens of
  ("I" : ts : msg) -> LogMessage Info (read ts) (unwords msg)
  ("W" : ts : msg) -> LogMessage Warning (read ts) (unwords msg)
  ("E" : s : ts : msg) -> LogMessage (Error (read s)) (read ts) (unwords msg)
  _ -> Unknown line
  where
    tokens = words line

-- | Parse a whole log file. 
parse :: String -> [LogMessage]
parse = map parseMessage . lines

{- Exercise 2 -}


-- | Insert a @LogMessage@ into a (sorted) @MessageTree@.
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert msg (Node l m r)
  | timeStamp msg < timeStamp m = Node (insert msg l) m r
  | otherwise = Node l m (insert msg r)

{- Exercise 3 -}

-- | Build a sorted @MessageTree@ from a list of @LogMessages@.
build :: [LogMessage] -> MessageTree
build = foldl (\tree msg -> insert msg tree) Leaf

{- Exercise 4 -}

-- | Perform an /in-order/ traversal of a @MessageTree@ and produce a list of
-- @LogMessage@'s sorted by timestamp.
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l m r) = inOrder l ++ [m] ++ inOrder r

{- Exercise 5 -}

-- | Take an unsorted @[LogMessage]@ and return a list of error messages with a
-- severity @(>= 50)@, sorted by timestamp.
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong =
  map logMsg
    . sortMessages
    . filter isSevereMsg
  where
    isSevereMsg (LogMessage (Error s) _ _) = s >= 50
    isSevereMsg _ = False
    sortMessages = inOrder . build
