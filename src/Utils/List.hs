module Utils.List where

-- | If the given list is prefixed by the given prefix, it will return the remainder. 
--   Otherwise, it will return Nothing
consumePrefix :: Eq a => [a] -> [a] -> Maybe [a]
consumePrefix str prefix
    | str `hasPrefix` prefix = Just $ drop (length prefix) str
    | otherwise = Nothing

-- | Checks if a given list has a specified prefix.
hasPrefix :: Eq a => [a] -> [a] -> Bool
hasPrefix str prefix = take (length prefix) str == prefix

-- | If the given list is prefixed by the given prefix, it will return the remainder. 
--   Otherwise, it will return Nothing
consumePostfix :: Eq a => [a] -> [a] -> Maybe [a]
consumePostfix str fix
    | str `hasPostfix` fix = Just $ take (length str - length fix) str
    | otherwise = Nothing

-- | Checks if a given list has a specified prefix.
hasPostfix :: Eq a => [a] -> [a] -> Bool
hasPostfix str fix = drop (length str - length fix) str == fix

-- | Splits the list on the first occurrence of a specified element.
splitOnFirst :: Eq a => [a] -> a -> ([a], [a])
splitOnFirst [] _ = ([], [])
splitOnFirst xs x = (takeWhile (/=x) xs, drop 1 $ dropWhile (/=x) xs)

splitOnElement :: Eq a => [a] -> a -> [[a]]
splitOnElement xs x = foldr f [[]] xs
    where
        f el (r:rs) 
            | el == x = []:r:rs
            | otherwise = (el:r):rs
