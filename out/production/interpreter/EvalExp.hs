module EvalExp where
import Parser
import DTCleaner
import Stmt

evalRawExp = do
    contents <- getContents
    putStrLn $ show $ evalExp $ semPExp $ getTree contents
