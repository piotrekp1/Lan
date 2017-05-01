module AbstractSyn where
import Parser

syn = do
    contents <- getContents
    putStrLn $  getTree contents
