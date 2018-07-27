module BinaryTree where

-- Given the BinaryTree from last chapter, complete the following exercises. Here’s that datatype again:

data BinaryTree a =
    Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

-- 1. Write unfold for BinaryTree.
unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f x = case f x of
    Nothing -> Leaf
    Just (a, b, c) -> Node (unfold f a) b (unfold f c)

{-2. Make a tree builder.
Using the unfold function you’ve made for BinaryTree, write the following function:
treeBuild :: Integer -> BinaryTree Integer
treeBuild n = undefined
-}
treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold treeHandler 0
    where
        treeHandler :: Integer -> Maybe (Integer, Integer, Integer)
        treeHandler x
            | x < n     = Just (x + 1, x, x + 1)
            | otherwise = Nothing


