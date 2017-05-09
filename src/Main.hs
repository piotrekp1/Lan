module Main where
import ExpEvaluator
import StaticChecker
import Utils
import Gramma
import Tokens
import DTCleaner


main = do
     contents <- getContents
     let abstractSyn = semPBlock $ lanParse $ lanTokens contents
     putStrLn $ show abstractSyn
     putStrLn $  "\n\n" ++ " ---------- "
     let env = [("x", 0), ("y", 1) , ("z", 2)]
     let res = execStoreWithEnv (checkExp' abstractSyn)
     case res of
         Left message -> do
             putStrLn ("Type Error: " ++ message)
         Right ((dt, store), messages) ->  do
             mapM_ putStrLn messages
             let res2 = execStoreWithEnv (evalExp' abstractSyn)
             case res2 of
                 Left message -> putStrLn ("Runtime Error: " ++ message)
                 Right ((dt, store), messages) -> do
                     mapM_ putStrLn messages
                     showStore store
     putStrLn $ " ---------- " ++ "\n\n"
