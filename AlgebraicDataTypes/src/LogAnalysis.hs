module LogAnalysis (parseMessage, parse)  where

import Log

parseMessage :: String -> LogMessage
parseMessage ('I':msg) = parseInfo (words msg)
parseMessage ('W':msg) = parseWarning (words msg)
parseMessage ('E':msg) = parseError (words msg)
parseMessage msg = Unknown msg

parseInfo :: [String] -> LogMessage
parseInfo (tsStr:msgA) = LogMessage Info ts text
  where
    ts = read tsStr :: Int
    text = unwords msgA

parseWarning :: [String] -> LogMessage
parseWarning (tsStr:msgA) = LogMessage Warning ts text
  where
    ts = read tsStr :: Int
    text = unwords msgA

parseError :: [String] -> LogMessage
parseError (lvlStr:tsStr:msgA) = LogMessage (Error level) ts text
  where
    ts = read tsStr :: Int
    level = read lvlStr :: Int
    text = unwords msgA

parse :: String -> [LogMessage]
parse msg = map parseMessage (lines msg)
