{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import ExpEvaluator
import StaticChecker
import Utils
import Gramma
import Tokens
import DTCleaner
import System.Environment
import SemanticDatatypes
import qualified Data.Map as DMap
import System.IO
import qualified Control.Exception as Exc

data Mod
     = PrintStore
     | PrintSemTree
     | Help
     | WrongMod
     deriving (Eq, Show)


getMod :: String -> Mod
getMod "-s" = PrintStore
getMod "-t" = PrintSemTree
getMod "-h" = Help
getMod c = WrongMod


helpMode :: IO()
helpMode = do
    putStrLn "usage: Lan [-sth] [filepath]"
    putStrLn "   filepath  :  path of the file with code to interpret, if not given then Lan shell is launched "
    putStrLn "   -s        :  prints Store after every computation"
    putStrLn "   -t        :  prints semantic tree of the computations"
    putStrLn "   -h        :  help"
printerSeparator :: String
printerSeparator =  "\n" ++ " ---------- " ++ "\n"

compilator :: [Mod] -> String-> IO()
compilator mods file = withFile file ReadMode (\handle -> do
        code <- hGetContents handle
        run [] DMap.empty mods code
        return ()
        )

run :: Env -> Store -> [Mod] -> String -> IO((Env, Store))
run env store mods code = Exc.catch (do
    let codeSem = semPBlock $ lanParse $ lanTokens code
    if elem PrintSemTree mods then putStrLn $ ((show codeSem) ++ printerSeparator )else return ()
    let res = checkBlock env store codeSem
    case res of
        Left message -> do
            hPutStrLn stderr message
            return (env, store)
        Right ((tp, check_store), messages) ->do
            case runBlock env store codeSem of
                Left message -> do
                    hPutStrLn stderr message
                    return (env, store)
                Right ((newEnv, newStore), messages) -> do
                    mapM_ putStrLn messages
                    if elem PrintStore mods then showStore newStore  else return()
                    return (newEnv, newStore)
        ) (\(e  :: Exc.SomeException)-> do
            hPutStrLn stderr $ show e
            return (env, store)
            )


interpreter :: Env -> Store -> [Mod] -> IO()
interpreter env store mods = do
    line <- getSentence ""
    (newEnv, newStore) <- run env store mods line
    interpreter newEnv newStore mods

count :: Char -> String -> Int
count c = length . (filter (==c))

getSentence :: String -> IO(String)
getSentence str = do
    newLine <- getLine
    let newSentence = str ++ newLine
    let (ob_cnt, cb_cnt) = (count '{' newSentence, count '}' newSentence)
    if ob_cnt == cb_cnt then return newSentence else getSentence newSentence

main :: IO()
main = do
    args <- getArgs
    let mod_strs = filter (\(c:rs) -> c == '-') args
    let files = filter (\(c:rs) -> c /= '-') args
    let mods = map getMod mod_strs
    if length files > 1 then fail "only one file at once" else return ()
    if elem WrongMod mods then fail "wrong modificator as argument" else return ()
    if elem Help mods then helpMode else (if length files == 1 then compilator mods (head files) else interpreter [] DMap.empty mods)
