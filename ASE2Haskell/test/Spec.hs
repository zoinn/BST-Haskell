import BinaryTree
import Data.List (nub, sort)
import Test.HUnit
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

-- MAIN
main :: IO ()
main = do
  _ <- runTestTT allTests
  defaultMain propertyTests
  return ()

-- HUNIT TEST VARIABLES
testTree :: Tree Int String
testTree = Node 5 "Root" (Node 3 "left" Null Null) (Node 10 "right" (Node 6 "Lefty" Null (Node 9 "inheritor" Null Null)) (Node 14 "Righty" Null Null))

expectedInsert1 = (Node 5 "Root" (Node 3 "left" Null Null) (Node 10 "right" (Node 6 "Lefty" Null (Node 9 "inheritor" Null Null)) (Node 14 "Righty" (Node 11 "insertionTest" Null Null) Null)))

expectedInsert2 = (Node 5 "Root" (Node 3 "left" Null Null) (Node 10 "right" (Node 6 "newLefty" Null (Node 9 "inheritor" Null Null)) (Node 14 "Righty" Null Null)))

expectedTraversal = [(3, "left"), (5, "Root"), (6, "Lefty"), (9, "inheritor"), (10, "right"), (14, "Righty")]

expectedRemove = (Node 5 "Root" (Node 3 "left" Null Null) (Node 14 "Righty" (Node 6 "Lefty" Null (Node 9 "inheritor" Null Null)) Null))

oddTree = (Node 5 "Root" (Node 3 "left" Null Null) (Node 9 "inheritor" Null Null))

evenTree = (Node 10 "right" (Node 6 "Lefty" Null Null) (Node 14 "Righty" Null Null))

leftRotation = Node 10 "right" (Node 5 "Root" (Node 3 "left" Null Null) (Node 6 "Lefty" Null (Node 9 "inheritor" Null Null))) (Node 14 "Righty" Null Null)

rightRotation = Node 3 "left" Null (Node 5 "Root" Null (Node 10 "right" (Node 6 "Lefty" Null (Node 9 "inheritor" Null Null)) (Node 14 "Righty" Null Null)))

-- HUnit TESTS
allTests :: Test
allTests =
  TestList
    [ TestCase (assertEqual "Can create binary tree with root node" (Node 3 "Newnode" Null Null) (createTree 3 "Newnode")),
      TestCase (assertEqual "Can search binary tree and find value" (Node 6 "Lefty" Null (Node 9 "inheritor" Null Null)) (searchTree 6 testTree)),
      TestCase (assertEqual "Can search binary tree and not find value" (Null) (searchTree 100 testTree)),
      TestCase (assertEqual "Can insert into binary tree" (expectedInsert1) (insert 11 "insertionTest" testTree)),
      TestCase (assertEqual "Can insert into binary tree with a value already present" (expectedInsert2) (insert 6 "newLefty" testTree)),
      TestCase (assertEqual "Can produce an in-order traversal of a binary tree" (expectedTraversal) (inOrderTraversal testTree)),
      TestCase (assertEqual "Can remove the node of a binary tree" (expectedRemove) (remove 10 testTree)),
      TestCase (assertEqual "Can find inheritor when removing node of a binary tree" (14, "Righty") (findInheritor (searchTree 14 testTree))),
      TestCase (assertEqual "Can remove all even integer keys from tree" (oddTree) (removeIf testTree isEven)),
      TestCase (assertEqual "Can remove all odd integer keys from tree" (evenTree) (removeIf testTree isOdd)),
      TestCase (assertEqual "Can perform a left binary tree rotation" (leftRotation) (rotateLeft 5 testTree)),
      TestCase (assertEqual "Can perform a right binary tree rotation" (rightRotation) (rotateRight 5 testTree))
    ]

-- QuickCheck instance for Tree to generate with property tests
-- We assume that the insert function works as intended to create arbritary trees for use in property testing

instance (Arbitrary key, Arbitrary value, Ord key) => Arbitrary (Tree key value) where
  arbitrary = arbitraryTree

arbitraryTree :: (Arbitrary key, Arbitrary value, Ord key) => Gen (Tree key value)
arbitraryTree = sized treeOfSize
  where
    treeOfSize 0 = return Null
    treeOfSize n = do
      key <- arbitrary
      value <- arbitrary
      tree <- resize (n - 1) arbitraryTree
      return (insert key value tree)

-- QuickCheck TESTS

prop_insertSearch :: Int -> String -> Tree Int String -> Bool
prop_insertSearch key value tree =
  let updatedTree = insert key value tree
      (Node k v _ _) = searchTree key updatedTree
   in key == k && value == v

prop_removeSearch :: Int -> Tree Int String -> Bool
prop_removeSearch key tree =
  let updatedTree = remove key tree
   in searchTree key updatedTree == Null

prop_insertRemoveSearch :: Int -> String -> Tree Int String -> Bool
prop_insertRemoveSearch key value tree =
  let updatedTree = insert key value tree
      removedTree = remove key updatedTree
   in searchTree key removedTree == Null

prop_removeIfEven :: Int -> Tree Int String -> Bool
prop_removeIfEven key tree =
  let removedTree = removeIf tree isEven
      searchResult = searchTree key removedTree
   in if isEven key
        then searchResult == Null
        else True

prop_removeIfOdd :: Int -> Tree Int String -> Bool
prop_removeIfOdd key tree =
  let removedTree = removeIf tree isOdd
      searchResult = searchTree key removedTree
   in if isOdd key
        then searchResult == Null
        else True

propertyTests :: TestTree
propertyTests =
  testGroup
    "QuickCheck Property Tests: "
    [ testProperty "Insert then search for the inserted key" prop_insertSearch,
      testProperty "Remove then search for the removed key" prop_removeSearch,
      testProperty "Insert then remove the inserted key then search for it" prop_insertRemoveSearch,
      testProperty "Remove even keys and check if they exist" prop_removeIfEven,
      testProperty "Remove odd Keys and check if they exist" prop_removeIfOdd
    ]