module Main where

import System.IO

import Data.List as L
import Data.Map  as M
import Data.Set  as S


filterWords :: [String] -> String -> Set String
filterWords allWords word =
    S.fromList [w | w <- allWords, length w == length word]

isSimilar :: String -> String -> Bool
isSimilar x y = length [1 | (a, b) <- zip x y, a /= b] == 1

buildPath :: String -> String -> Map String String -> [String] -> [String]
buildPath start end path [] = buildPath start end path [end]
buildPath start end path list@(first:_)
    | first == start = list
    | otherwise = buildPath start end path (path ! first : list)

bfs :: String -> String -> Set String -> Map String String
bfs start end ws = bfs' start end ws [start] (M.singleton start "")

bfs' :: String -> String -> Set String -> [String] -> Map String String -> Map String String
bfs' start end ws queue@(current:qs) path = do

    if end `elem` keys path
        then path
        else if L.null queue
            then M.empty
            else do process
    where
        process = do
            let incSet = S.filter (\x -> x `isSimilar` current) ws
            let incList = S.toList incSet
            let p = M.union (getPairs current incList) path
            bfs' current end (S.difference ws incSet) (qs ++ incList) p

        getPairs word l = M.fromList (L.map (\x -> (x, word)) l)


main = do

    h <- openFile "runouns.txt" ReadMode
    hSetEncoding h utf8
    text <- hGetContents h
    let allWords = words text

    hSetEncoding stdout utf8

    start <- getLine
    if not $ start `elem` allWords
        then do error "I don't know this word" else do

    end <- getLine
    if not $ end `elem` allWords
        then do error "I don't know this word" else do

    if not $ length start == length end
        then do error "Words must be the same length" else do

    let ws = filterWords allWords start
    let path = bfs start end ws
    let result = buildPath start end path []

    mapM_ putStrLn result

    let resLen = length result
    if resLen == 0
        then putStrLn "Cannot find a path"
        else putStrLn $ "Count: " ++ show resLen

    hClose h
