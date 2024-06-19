module BinaryTree where

data Tree key value = Node key value (Tree key value) (Tree key value) | Null deriving (Eq, Show)

createTree :: key -> value -> Tree key value
createTree key value = Node key value Null Null

searchTree :: (Ord key) => key -> Tree key value -> Tree key value
searchTree _ Null = Null
searchTree soughtKey (Node key value left right)
  | soughtKey == key = Node key value left right
  | soughtKey < key = searchTree soughtKey left
  | otherwise = searchTree soughtKey right

insert :: (Ord key) => key -> value -> Tree key value -> Tree key value
insert key value Null = Node key value Null Null
insert soughtKey newValue (Node key value left right)
  | soughtKey == key = Node key newValue left right
  | soughtKey < key = Node key value (insert soughtKey newValue left) right
  | otherwise = Node key value left (insert soughtKey newValue right)

inOrderTraversal :: Tree key value -> [(key, value)]
inOrderTraversal tree = inOrderTraversalHelper tree []

inOrderTraversalHelper :: Tree key value -> [(key, value)] -> [(key, value)]
inOrderTraversalHelper Null traversal = traversal
inOrderTraversalHelper (Node key value left right) traversal =
  inOrderTraversalHelper left traversal
    ++ [(key, value)]
    ++ inOrderTraversalHelper right []

remove :: (Ord key) => key -> Tree key value -> Tree key value
remove _ Null = Null
remove key (Node k value left right)
  | key < k = Node k value (remove key left) right
  | key > k = Node k value left (remove key right)
  | otherwise = removeWorker (Node k value left right)

removeWorker :: (Ord key) => Tree key value -> Tree key value
removeWorker (Node _ _ Null right) = right
removeWorker (Node _ _ left Null) = left
removeWorker (Node _ _ left right) = Node key value left (remove key right) where (key, value) = findInheritor right

findInheritor :: Tree key value -> (key, value)
findInheritor (Node key value _ _) = (key, value)
findInheritor (Node _ _ left _) = findInheritor left

removeIf :: (Ord key) => Tree key value -> (key -> Bool) -> Tree key value
removeIf Null _ = Null
removeIf (Node key value l r) predicate =
  let leftSubtree = removeIf l predicate
      rightSubtree = removeIf r predicate
      newNode = Node key value leftSubtree rightSubtree
   in if predicate key
        then remove key newNode
        else newNode

isEven :: Int -> Bool
isEven key = even key

isOdd :: Int -> Bool
isOdd key = odd key

rotateLeft :: (Ord key) => key -> Tree key value -> Tree key value
rotateLeft _ Null = Null
rotateLeft pivot oldTree = newTree
  where
    (Node key value (Node kLeft vLeft lChildLeft rChildLeft) (Node kRight vRight lChildRight rChildRight)) = searchTree pivot oldTree
    newTree = (Node kRight vRight (Node key value (Node kLeft vLeft lChildLeft rChildLeft) lChildRight) rChildRight)

rotateRight :: (Ord key) => key -> Tree key value -> Tree key value
rotateRight _ Null = Null
rotateRight pivot oldTree = newTree
  where
    (Node key value (Node kLeft vLeft lChildLeft rChildLeft) (Node kRight vRight lChildRight rChildRight)) = searchTree pivot oldTree
    newTree = (Node kLeft vLeft lChildLeft (Node key value rChildLeft (Node kRight vRight lChildRight rChildRight)))
