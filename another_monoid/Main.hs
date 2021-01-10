import Test.Hspec
import Test.QuickCheck
import Data.Monoid

data Optional a = Nada | Only a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  Nada <> b = b
  a <> Nada = a
  (Only a) <> (Only b) = Only (a <> b)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada


newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance Semigroup (First' a) where
  (First' Nada) <> b = b
  a <> _ = a

instance Monoid (First' a) where
  mempty = First' Nada



monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = First' <$> arbitrary

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary =
    oneof [ return Nada
          , Only <$> arbitrary
          ]

type FirstMappend = First' String -> First' String -> First' String -> Bool

type FstId = First' String -> Bool

main = hspec $ do
  it "associativity" $ do
    property (monoidAssoc :: FirstMappend)
  it "left identity" $ do
    property (monoidLeftIdentity :: FstId)
  it "right identity" $ do
    property (monoidRightIdentity :: FstId)
