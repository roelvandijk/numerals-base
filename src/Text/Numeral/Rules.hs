{-# LANGUAGE NoImplicitPrelude
           , PackageImports
           , UnicodeSyntax
  #-}

{-|

Rules to convert numbers to an expression language.

-}
module Text.Numeral.Rules
  ( -- * The Rule type
    Rule

    -- * Rule combinators
  , conditional
  , combine
  , mapRule
  , findRule

    -- * Rules
  , unknown

  , lit, lit1

  , pos, checkPos
  , dual, plural

  , add
  , mul, mul1
  , sub

  , mulScale, mulScale1
  , shortScale,  longScale,  pelletierScale
  , shortScale1, longScale1, pelletierScale1

  , mkStep, step, step1
  ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import "base" Data.Bool           ( Bool, otherwise )
import "base" Data.Function       ( ($), id, const, flip, fix )
import "base" Data.List           ( foldr )
import "base" Data.Ord            ( Ord, (<), (>) )
import "base" Prelude             ( Integral, fromIntegral
                                  , Num, (-), abs, divMod, div, even
                                  )
import "base-unicode-symbols" Data.Eq.Unicode       ( (≡) )
import "base-unicode-symbols" Data.Function.Unicode ( (∘) )
import "base-unicode-symbols" Prelude.Unicode       ( (⋅) )
import "this"                 Text.Numeral.Exp      ( Side(L, R) )
import "this"                 Text.Numeral.Misc     ( intLog )
import qualified "this"       Text.Numeral.Exp.Classes as C
import qualified "fingertree" Data.IntervalMap.FingerTree as FT
    ( Interval(Interval)
    , IntervalMap, empty, insert
    , search
    )


--------------------------------------------------------------------------------
-- The Rule type
--------------------------------------------------------------------------------

-- | A rule on how to convert a number into an expression
-- language. Notice how this type is equal to the type of the '$'
-- operator.
type Rule α β = (α → β) → (α → β)


--------------------------------------------------------------------------------
-- Rule combinators
--------------------------------------------------------------------------------


-- | The \'if-then-else\' concept for rules. Applies the first rule if
-- the predicate holds on the input value, otherwise applies the
-- second rule.
conditional ∷ (α → Bool) -- ^ Predicate on input value (\"if\").
            → Rule α β -- ^ Rule to apply when predicate holds (\"then\").
            → Rule α β -- ^ Rule to apply when predicate does not hold (\"else\").
            → Rule α β
conditional p t e = \f n → if p n
                           then t f n
                           else e f n

-- | Tries to apply the first rule, if that produces an 'C.unknown'
-- value it applies the second rule.
combine ∷ (C.Unknown β)
        ⇒ Rule α β
        → Rule α β
        → Rule α β
combine r1 r2 = \f n → case r1 f n of
                         x | C.isUnknown x → r2 f n
                           | otherwise     → x

-- | Transform a value before it is given to a rule.
mapRule ∷ (α → α) → Rule α β → Rule α β
mapRule g r = \f n → r f (g n)

-- | Chooses which rule to apply to an input value based on a interval
-- list of rules.
findRule ∷ (Ord α, Num α, C.Unknown β)
         ⇒ (α, Rule α β)   -- ^ First interval rule.
         → [(α, Rule α β)] -- ^ Interval rule list.
         → α               -- ^ Upper bound of the last interval.
         → Rule α β
findRule x xs end = \f n → case FT.search n xm of
                             [] → C.unknown
                             (_,r):_ → r f n
    where
      xm = mkIntervalMap $ mkIntervalList x xs end


--------------------------------------------------------------------------------
-- Rules
--------------------------------------------------------------------------------

-- | A rule that always fails to convert a value. It constantly
-- produces the 'C.unknown' value.
--
-- >>> (fix unknown) (3 :: Integer) :: Exp
-- Unknown
unknown ∷ (C.Unknown β) ⇒ Rule α β
unknown _ _ = C.unknown

-- | The literal rule. Converts its argument into a 'C.lit'eral
-- expression.
--
-- >>> lit (fix unknown) (3 :: Integer) :: Exp
-- Lit 3
--
-- In this example lit is applied to the nonsense rule \"'fix'
-- 'unknown'\". Lit ignores that function, which is why we can pass it
-- anything we want, including itself.
--
-- >>> lit (fix undefined) (3 :: Integer) :: Exp
-- Lit 3
-- >>> (fix lit) (3 :: Integer) :: Exp
-- Lit 3
lit ∷ (Integral α, C.Lit β) ⇒ Rule α β
lit = const $ C.lit ∘ fromIntegral

-- | A variant on the 'lit' rule which always multiplies its argument
-- with 1. Useful for languages which have numerals of the form \"one
-- hundred and three\" as opposed to \"hundred and three\".
--
-- >>> lit1 (fix unknown) (3 :: Integer) :: Exp
-- Mul (Lit 1) (Lit 3)
lit1 ∷ (Integral α, C.Lit β, C.Mul β) ⇒ Rule α β
lit1 = const $ \n → C.lit 1 `C.mul` C.lit (fromIntegral n)

-- |
--
-- >>> (pos $ lit $ fix unknown) (3 :: Integer) :: Exp
-- Lit 3
-- >>> (pos $ lit $ fix unknown) (-3 :: Integer) :: Exp
-- Neg (Lit 3)
pos ∷ (Ord α, Num α, C.Lit β, C.Neg β) ⇒ Rule α β
pos f n | n < 0     = C.neg $ f (abs n)
        | n > 0     = f n
        | otherwise = C.lit 0

-- |
--
-- >>> (checkPos $ lit $ fix unknown) (3 :: Integer) :: Exp
-- Lit 3
-- >>> (checkPos $ lit $ fix unknown) (-3 :: Integer) :: Exp
-- Unknown
checkPos ∷ (Ord α, Num α, C.Unknown β, C.Lit β) ⇒ Rule α β
checkPos f n | n < 0     = C.unknown
             | n > 0     = f n
             | otherwise = C.lit 0

-- |
--
-- >>> (dual $ lit $ fix unknown) (3 :: Integer) :: Exp
-- Dual (Lit 3)
dual ∷ (C.Dual β) ⇒ Rule α β
dual = (∘) C.dual

-- |
--
-- >>> (plural $ lit $ fix unknown) (3 :: Integer) :: Exp
-- Plural (Lit 3)
plural ∷ (C.Plural β) ⇒ Rule α β
plural = (∘) C.plural

-- |
--
-- >>> (add 10 L $ lit $ fix unknown) (13 :: Integer) :: Exp
-- Add (Lit 3) (Lit 10)
add ∷ (Num α, C.Add β) ⇒ α → Side → Rule α β
add val s = \f n → (flipIfR s C.add) (f $ n - val) (f val)

-- |
--
-- >>> (mul 10 R L $ lit $ fix unknown) (42 :: Integer) :: Exp
-- Add (Mul (Lit 4) (Lit 10)) (Lit 2)
mul ∷ (Integral α, C.Add β, C.Mul β) ⇒ α → Side → Side → Rule α β
mul val aSide mSide =
    \f n → let (m, a) = n `divMod` val
               mval = (flipIfR mSide C.mul) (f m) (f val)
           in if a ≡ 0
              then mval
              else (flipIfR aSide C.add) (f a) mval

mul1 ∷ (Integral α, C.Lit β, C.Add β, C.Mul β)
     ⇒ α → Side → Side → Rule α β
mul1 val aSide mSide =
    \f n → let (m, a) = n `divMod` val
               mval = if m ≡ 1
                      then C.lit 1 ⊡ C.lit (fromIntegral val)
                      else f m ⊡ C.lit (fromIntegral val)
           in if a ≡ 0
              then mval
              else (flipIfR aSide C.add) (f a) mval
  where
     (⊡) = flipIfR mSide C.mul

-- |
--
-- >>> (sub 20 $ lit $ fix unknown) (18 :: Integer) :: Exp
-- Sub (Lit 2) (Lit 20)
sub ∷ (Integral α, C.Sub β) ⇒ α → Rule α β
sub val = \f n → C.sub (f $ val - n) (f val)

mkStep ∷ (Integral α, C.Unknown β, C.Lit β, C.Add β, C.Mul β)
       ⇒ Rule α β                     -- ^ lit rule
       → (α → Side → Rule α β)        -- ^ add rule
       → (α → Side → Side → Rule α β) -- ^ mul rule
       → α → α → Side → Side → Rule α β
mkStep lr ar mr val r aSide mSide
       f n | n < val   = C.unknown
           | n ≡ val   = lr                 f n
           | n < val⋅2 = ar val aSide       f n
           | n < val⋅r = mr val aSide mSide f n
           | otherwise = C.unknown

step ∷ (Integral α, C.Unknown β, C.Lit β, C.Add β, C.Mul β)
     ⇒ α → α → Side → Side → Rule α β
step = mkStep lit add mul

step1 ∷ (Integral α, C.Unknown β, C.Lit β, C.Add β, C.Mul β)
      ⇒ α → α → Side → Side → Rule α β
step1 = mkStep lit1 add mul1

-- See: http://en.wikipedia.org/wiki/Names_of_large_numbers
mulScale ∷ (Integral α, C.Scale α, C.Add β, C.Mul β, C.Scale β)
         ⇒ α → α → Side → Side → Rule α β → Rule α β
mulScale base offset aSide mSide bigNumRule =
    \f n → let rank    = (intLog n - offset) `div` base
               base'   = fromIntegral base
               offset' = fromIntegral offset
               rank'   = fromIntegral rank
               rankExp = (fix bigNumRule) rank
               (m, a)  = n `divMod` C.scale base' offset' rank'
               scale'  = C.scale base' offset' rankExp
               mval | m ≡ 1     = scale'
                    | otherwise = (flipIfR mSide C.mul)
                                  (f m)
                                  scale'
           in if a ≡ 0
              then mval
              else (flipIfR aSide C.add) (f a) mval

mulScale1 ∷ (Integral α, C.Scale α, C.Add β, C.Mul β, C.Scale β)
          ⇒ α → α → Side → Side → Rule α β → Rule α β
mulScale1 base offset aSide mSide bigNumRule =
    \f n → let rank    = (intLog n - offset) `div` base
               base'   = fromIntegral base
               offset' = fromIntegral offset
               rank'   = fromIntegral rank
               rankExp = (fix bigNumRule) rank
               (m, a)  = n `divMod` C.scale base' offset' rank'
               mval    = (flipIfR mSide C.mul)
                         (f m)
                         (C.scale base' offset' rankExp)
           in if a ≡ 0
              then mval
              else (flipIfR aSide C.add) (f a) mval

shortScale ∷ (Integral α, C.Scale α, C.Add β, C.Mul β, C.Scale β)
           ⇒ Side → Side → Rule α β → Rule α β
shortScale = mulScale 3 3

shortScale1 ∷ (Integral α, C.Scale α, C.Add β, C.Mul β, C.Scale β)
            ⇒ Side → Side → Rule α β → Rule α β
shortScale1 = mulScale1 3 3

longScale ∷ (Integral α, C.Scale α, C.Add β, C.Mul β, C.Scale β)
          ⇒ Side → Side → Rule α β → Rule α β
longScale = mulScale 6 0

longScale1 ∷ (Integral α, C.Scale α, C.Add β, C.Mul β, C.Scale β)
           ⇒ Side → Side → Rule α β → Rule α β
longScale1 = mulScale1 6 0

pelletierScale ∷ (Integral α, C.Scale α, C.Add β, C.Mul β, C.Scale β)
                ⇒ Side → Side → Rule α β → Rule α β
pelletierScale aSide mSide bigNumRule =
    conditional (\n → even $ intLog n `div` 3)
                (mulScale 6 0 aSide mSide bigNumRule)
                (mulScale 6 3 aSide mSide bigNumRule)

pelletierScale1 ∷ (Integral α, C.Scale α, C.Add β, C.Mul β, C.Scale β)
                ⇒ Side → Side → Rule α β → Rule α β
pelletierScale1 aSide mSide bigNumRule =
    conditional (\n → even $ intLog n `div` 3)
                (mulScale1 6 0 aSide mSide bigNumRule)
                (mulScale1 6 3 aSide mSide bigNumRule)


--------------------------------------------------------------------------------
-- Miscellaneous
--------------------------------------------------------------------------------

flipIfR ∷ Side → (α → α → α) → (α → α → α)
flipIfR L = id
flipIfR R = flip

mkIntervalList ∷ (Num a) ⇒ (a, b) → [(a, b)] → a → [((a, a), b)]
mkIntervalList (k, r) krs end = go k r krs
    where
      go k1 r1 []            = [((k1, end), r1)]
      go k1 r1 ((k2, r2):xs) = ((k1, k2-1), r1) : go k2 r2 xs

mkIntervalMap ∷ (Ord v) ⇒ [((v, v), α)] → FT.IntervalMap v α
mkIntervalMap = foldr ins FT.empty
  where ins ((lo, hi), n) = FT.insert (FT.Interval lo hi) n

