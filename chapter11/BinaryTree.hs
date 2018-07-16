module BinaryTree where

{-
a) we need Ord in order to have information about our values to know how to arrange them in the tree.
Left lesser, right greater is a common convention for arranging binary trees
-}
data BinaryTree a =
    Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
    | b == a = Node left a right
    | b < a = Node (insert' b left) a right
    | b > a = Node left a (insert' b right)


mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' =
    Node (Node Leaf 3 Leaf)
    1
    (Node Leaf 4 Leaf)

mapExpected :: BinaryTree Integer
mapExpected =
    Node (Node Leaf 4 Leaf)
    2
    (Node Leaf 5 Leaf)

-- acceptance test for mapTree
mapOkay :: IO()
mapOkay =
    if mapTree (+1) testTree' == mapExpected
    then print "yup okay!"
    else error "test failed!"

-- ########## Converting binary trees to lists

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = [a] ++ preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = inorder left ++ [a] ++ inorder right

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = postorder left ++ postorder right ++ [a]

testTree :: BinaryTree Integer
testTree =
    Node (Node Leaf 1 Leaf)
    2
    (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
    if preorder testTree == [2, 1, 3]
    then putStrLn "Preorder fine!"
    else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
    if inorder testTree == [1, 2, 3]
    then putStrLn "Inorder fine!"
    else putStrLn "Bad news bears."

-- I added this
testPostorder :: IO ()
testPostorder =
    if postorder testTree == [1, 3, 2]
    then putStrLn "Postorder fine!"
    else putStrLn "Bad news bears."

-- ############# foldr 4 binary trees any traversal order is fine

-- Firs solution: use preorder (or in order or preorder) to map binaryTree into list, and then use classic foldr
-- Not too much elegant
foldTree1 :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree1 f c t = foldr f c ( preorder t )

-- that's it! 
foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ b Leaf = b
foldTree f b (Node left a right) = foldTree f ( f a ( foldTree f b left) ) right