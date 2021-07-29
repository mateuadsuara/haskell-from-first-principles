{-# LANGUAGE DeriveFunctor #-}

data IntExpressionF r = ConstF Int
                      | AddF r r
                      | SubF r r
                      | MulF r r
                      | DivF r r deriving Functor

newtype Fix f = In { out :: f (Fix f) }
type Alg f e = f e -> e

cata :: Functor f => Alg f e -> Fix f -> e
cata f = f . fmap (cata f) . out

pretty :: Fix IntExpressionF -> String
pretty = cata p
  where p (ConstF n) = show n
        p (AddF l r) = p' l r "+"
        p (SubF l r) = p' l r "-"
        p (MulF l r) = p' l r "*"
        p (DivF l r) = p' l r "/"
        p' l r s = concat ["(", l, s, r, ")"]

prettyWithSmartParenthesis :: Fix IntExpressionF -> String
prettyWithSmartParenthesis = fst . cata p
  where p (ConstF n) = (show n, 2)
        p (AddF tl tr) = p' tl tr "+" 0
        p (SubF tl tr) = p' tl tr "-" 0
        p (MulF tl tr) = p' tl tr "*" 1
        p (DivF tl tr) = p' tl tr "/" 1
        p' (l, pl) (r, pr) s priority = (concat [ parl, s, parr ], priority)
            where parenthesize e pe pr right | pe < pr || (pe == pr && right) = "(" ++ e ++ ")"
                                             | otherwise = e
                  parl = parenthesize l pl priority False
                  parr = parenthesize r pr priority True
