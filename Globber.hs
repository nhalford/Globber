-- Noah Halford
-- CMSC 22311
-- May 19, 2015
-- This code is available at https://github.com/nhalford/Globber

module Globber (matchGlob) where

type GlobPattern = String

matchGlob :: GlobPattern -> String -> Bool
matchGlob "" "" = True
matchGlob "*" _ = True
matchGlob ('*':x:xs) (y:ys)
    | x == '?' = matchGlob ('*':xs) ys
    | x == '\\' = (head xs == head remaining) && (matchGlob (tail xs) (tail remaining))
    | otherwise = matchGlob (x:xs) (dropWhile (/= x) (y:ys))
    where remaining = dropWhile (/= head xs) (y:ys)
matchGlob ('\\':x:xs) (y:ys) = (x == y) && matchGlob xs ys
matchGlob ('?':xs) (_:ys) = matchGlob xs ys
matchGlob (x:xs) (y:ys) = (x == y) && matchGlob xs ys
matchGlob _ _ = False
