{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

-- Ex 1

parseMessage :: String -> LogMessage
parseMessage s = parseMessageList(words s)

parseMessageList :: [String] -> LogMessage
parseMessageList []           = Unknown ""
parseMessageList [x]          = Unknown x
parseMessageList (x:xs:[])    = Unknown (x ++ xs)
parseMessageList ("I":ts:s)   = LogMessage Info
                              (read ts :: TimeStamp) (unwords s)
parseMessageList ("W":ts:s)   = LogMessage Warning
                                (read ts :: TimeStamp) (unwords s)
parseMessageList ("E":n:ts:s) = LogMessage (Error (read n :: Int))
                              (read ts :: TimeStamp) (unwords s)
parseMessageList m            = Unknown (unwords m)


parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)


-- Ex 2

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t            = t
insert _ (Node _ (Unknown _) _) = error "Tree cannot have Unknown log type"
insert m Leaf                   = Node Leaf m Leaf
insert m@(LogMessage _ t_i _) (Node tree_l n@(LogMessage _ t_c _) tree_r)
  | t_i < t_c                   = Node (insert m tree_l) n tree_r
  | otherwise                   = Node tree_l n (insert m tree_r)


-- Ex 3

build :: [LogMessage] -> MessageTree
build []     = Leaf
build (x:xs) = insert x (build xs)

-- Ex 4

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf                             = []
inOrder (Node (Leaf) m (Leaf))           = [m]
inOrder (Node (Leaf) m t_r@(Node _ _ _)) = m:(inOrder t_r)
inOrder (Node t_l m t_r)                 = (inOrder t_l) ++ [m] ++ (inOrder t_r)

-- Ex 5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong l  = whatWentWrongOrdered(inOrder(build l))

whatWentWrongOrdered :: [LogMessage] -> [String]
whatWentWrongOrdered []     = []
whatWentWrongOrdered ((LogMessage (Error n) _ s):ms)
  | n >= 50                 = s:(whatWentWrongOrdered ms)
  | otherwise               = whatWentWrongOrdered ms
whatWentWrongOrdered (_:ms) = whatWentWrongOrdered ms