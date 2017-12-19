module Util where

safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (x:xs) = Just x

deepMap :: (Functor f, Functor f1, Functor f2) => (a -> b) -> f2 (f1 (f a)) -> f2 (f1 (f b))
deepMap = fmap . fmap . fmap
