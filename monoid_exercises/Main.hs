import Test.Hspec (hspec, describe, it, shouldBe)
import Test.QuickCheck (property, Arbitrary(arbitrary), CoArbitrary, oneof)
import Data.Monoid
import Data.Semigroup

-- Optional

data Optional a = Nada | Only a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  Nada <> b = b
  a <> Nada = a
  (Only a) <> (Only b) = Only (a <> b)

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = oneof
    [ return Nada
    , Only <$> arbitrary
    ]

-- Trivial

data Trivial = Trivial
  deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

-- Identity

newtype Identity a = Identity a
  deriving (Eq, Show)

instance (Semigroup s) => Semigroup (Identity s) where
  (Identity a) <> (Identity b) = Identity (a <> b)

instance (Monoid m) => Monoid (Identity m) where
  mempty = Identity mempty

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

-- Two

data Two a b = Two a b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two x y) = Two (a <> x) (b <> y)

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

-- Three

data Three a b c = Three a b c
  deriving (Eq, Show)

instance (Semigroup c) => Semigroup (Three a b c) where
  (Three a b c) <> (Three x y z) = Three a y (c <> z)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

-- Four

data Four a b c d = Four a b c d
  deriving (Eq, Show)

instance Semigroup (Four a b c d) where
  a <> b = a

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

-- BoolConj

newtype BoolConj = BoolConj Bool
  deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj a) <> (BoolConj b) = BoolConj (a && b)

instance Monoid BoolConj where
  mempty = BoolConj True

instance Arbitrary BoolConj where
  arbitrary = BoolConj <$> arbitrary

-- BoolDisj

newtype BoolDisj = BoolDisj Bool
  deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj a) <> (BoolDisj b) = BoolDisj (a || b)

instance Monoid BoolDisj where
  mempty = BoolDisj False

instance Arbitrary BoolDisj where
  arbitrary = BoolDisj <$> arbitrary

-- Or

data Or a b = Fst a | Snd b
  deriving (Eq, Show)

instance Semigroup (Or a b) where
  a@(Snd b) <> _ = a
  _ <> b = b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = oneof
    [ Fst <$> arbitrary
    , Snd <$> arbitrary
    ]

-- Combine

newtype Combine a b = Combine { unCombine :: (a -> b) }

instance (Semigroup b) => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine (\n -> (f n) <> (g n))

instance (Monoid b) => Monoid (Combine a b) where
  mempty = Combine (\a -> mempty)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = Combine <$> arbitrary

instance Show (Combine a b) where
  show c = "Combine"

-- Comp

newtype Comp a = Comp { unComp :: (a -> a) }

instance Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp (f . g)

instance Monoid (Comp a) where
  mempty = Comp id

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = Comp <$> arbitrary

instance Show (Comp a) where
  show c = "Comp"

-- Validation

data Validation f s =
  Failure f | Success s
  deriving (Eq, Show)

instance (Semigroup f) => Semigroup (Validation f s) where
  (Failure f1) <> (Failure f2) = Failure (f1 <> f2)
  s@(Success _) <> _ = s
  _ <> s@(Success _) = s

instance (Arbitrary f, Arbitrary s) => Arbitrary (Validation f s) where
  arbitrary = oneof
    [ Success <$> arbitrary
    , Failure <$> arbitrary
    ]

-- Mem

newtype Mem s a = Mem { runMem :: s -> (a, s) }

instance (Semigroup a) => Semigroup (Mem s a) where
  (Mem f) <> (Mem g) = Mem $ \s ->
    let (a, s')  = f s
        (b, s'') = g s'
      in (a <> b, s'')

instance (Monoid a) => Monoid (Mem s a) where
  mempty = Mem (\s -> (mempty, s))

instance (CoArbitrary s, Arbitrary s, Arbitrary a) => Arbitrary (Mem s a) where
  arbitrary = Mem <$> arbitrary

instance Show (Mem s a) where
  show m  = "Mem"

-- Properties

lAssoc a b c = ((a <> b) <> c)
rAssoc a b c = (a <> (b <> c))
associativity eq a b c = (lAssoc a b c) `eq` (rAssoc a b c)

lId a = mempty `mappend` a
rId a = a `mappend` mempty
identity eq a = ((lId a) `eq` a) && ((rId a) `eq` a)

type PAssocV s = s -> s -> s -> Bool
pAssocV a b c = associativity (==) a b c

type PIdV m = m -> Bool
pIdV a = identity (==) a

unboxEqF un x a b = (un a) x == (un b) x

type PAssocF x s = x -> s -> s -> s -> Bool
pAssocF un x = associativity (unboxEqF un x)

type PIdF x m = x -> m -> Bool
pIdF un x = identity (unboxEqF un x)

-- Main

main = hspec $ do
  describe "Optional" $ do
    it "associativity" $ do
      property (pAssocV :: PAssocV (Optional String))
  describe "Trivial" $ do
    it "associativity" $ do
      property (pAssocV :: PAssocV Trivial)
    it "identity" $ do
      property (pIdV :: PIdV Trivial)
  describe "Identity" $ do
    it "associativity" $ do
      property (pAssocV :: PAssocV (Identity String))
    it "identity" $ do
      property (pIdV :: PIdV (Identity String))
  describe "Two" $ do
    it "associativity" $ do
      property (pAssocV :: PAssocV (Two String String))
    it "identity" $ do
      property (pIdV :: PIdV (Two String String))
  describe "Three" $ do
    it "associativity" $ do
      property (pAssocV :: PAssocV (Three String String String))
  describe "Four" $ do
    it "associativity" $ do
      property (pAssocV :: PAssocV (Four String String String String))
  describe "BoolConj" $ do
    it "associativity" $ do
      property (pAssocV :: PAssocV BoolConj)
    it "identity" $ do
      property (pIdV :: PIdV BoolConj)
  describe "BoolDisj" $ do
    it "associativity" $ do
      property (pAssocV :: PAssocV BoolDisj)
    it "identity" $ do
      property (pIdV :: PIdV BoolDisj)
  describe "Or" $ do
    it "associativity" $ do
      property (pAssocV :: PAssocV (Or String String))
    it "Fst 1 <> Snd 2" $ do
      (Fst 1) <> (Snd 2) `shouldBe` (Snd 2)
    it "Fst 1 <> Fst 2" $ do
      (Fst 1) <> (Fst 2) `shouldBe` ((Fst 2) :: (Or Int Int))
    it "Snd 1 <> Fst 2" $ do
      (Snd 1) <> (Fst 2) `shouldBe` (Snd 1)
    it "Snd 1 <> Snd 2" $ do
      (Snd 1) <> (Snd 2) `shouldBe` ((Snd 1) :: (Or Int Int))
  describe "Combine: f=Sum(n+1), g=Sum(n-1)" $
    let f = Combine $ \n -> Sum (n + 1)
        g = Combine $ \n -> Sum (n - 1)
      in do
      it "(f <> g) 0" $ do
        Sum 0 `shouldBe` (unCombine (f <> g) $ 0)
      it "(f <> g) 1" $ do
        Sum 2 `shouldBe` (unCombine (f <> g) $ 1)
      it "(f <> f) 1" $ do
        Sum 4 `shouldBe` (unCombine (f <> f) $ 1)
      it "(g <> f) 1" $ do
        Sum 2 `shouldBe` (unCombine (g <> f) $ 1)
      it "associativity" $ do
        property (pAssocF unCombine :: PAssocF Integer (Combine Integer (Sum Integer)))
      it "(mappend f mempty) 1" $ do
        Sum 2 `shouldBe` (unCombine (mappend f mempty) $ 1)
      it "identity" $ do
        property (pIdF unCombine :: PIdF Integer (Combine Integer (Sum Integer)))
  describe "Comp" $ do
    it "associativity" $ do
      property (pAssocF unComp :: PAssocF Integer (Comp Integer))
    it "identity" $ do
      property (pIdF unComp :: PIdF Integer (Comp Integer))
  describe "Validation" $ do
    it "associativity" $ do
      property (pAssocV :: PAssocV (Validation String Int))
    it "Success 1 <> Failure 'blah'" $ do
      (Success 1) <> (Failure "blah") `shouldBe` (Success 1)
    it "Failure 'woot' <> Failure 'blah'" $ do
      (Failure "woot") <> (Failure "blah") `shouldBe` ((Failure "wootblah") :: Validation String Int)
    it "Success 1 <> Success 2" $ do
      (Success 1) <> (Success 2) `shouldBe` ((Success 1) :: Validation String Int)
    it "Failure 'woot' <> Success 2" $ do
      (Failure "woot") <> (Success 2) `shouldBe` (Success 2)
  describe "Mem" $ do
    it "associativity" $ do
      property (pAssocF runMem :: PAssocF Integer (Mem Integer String))
    it "identity" $ do
      property (pIdF runMem :: PIdF Integer (Mem Integer String))
    let f'      = Mem (\s -> ("hi", s + 1))
        rmzero  = runMem mempty 0
        rmleft  = runMem (f' <> mempty) 0
        rmright = runMem (mempty <> f') 0
      in do
      it "rmleft" $ do
        ("hi", 1) `shouldBe` rmleft
      it "rmright" $ do
        ("hi", 1) `shouldBe` rmright
      it "rmzero" $ do
        ("", 0) `shouldBe` (rmzero :: (String, Int))
      it "rmleft == runMem f' 0" $ do
        True `shouldBe` (rmleft == runMem f' 0)
      it "rmright == runMem f' 0" $ do
        True `shouldBe` (rmright == runMem f' 0)
