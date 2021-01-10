import Test.Hspec
import Test.QuickCheck
import Data.Monoid

type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin' e adv noun adj =
  e    <> "! he said" <>
  adv  <> " as he jumped into his car " <>
  noun <> " and drove of with his " <>
  adj  <> " wife."

madlibbinBetter' e adv noun adj =
  mconcat
    [ e
    , "! he said"
    , adv
    , " as he jumped into his car "
    , noun
    , " and drove of with his "
    , adj
    , " wife."
    ]

main = hspec $ do
  it "" $ do
    property $ \e adv noun adj -> (madlibbin' e adv noun adj) == (madlibbinBetter' e adv noun adj)
