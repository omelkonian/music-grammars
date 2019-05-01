module Grammar.Types
       ( Double
       , Grammar (..), Rule (..), Head, Activation, Body
       , Term (..), Expand (..), Grammarly, Interpret(..)
       , runGrammar, always, (/\), (\/), (|->)
       ) where

import System.Random

import Music (Dur, Music(..), Music1, ToMusic1, toMusic1, Note1, pattern Note')

{- Operators' precedence. -}
infix  6 :%:
infix  5 :$:
infixr 4 :-:
infix  3 :->
infix  3 |->
infix  2 :|:

{- Grammar datatypes. -}
data Grammar meta a = a :|: [Rule meta a]
data Rule meta a = Head a :-> Body meta a
type Head a = (a, Double, Activation)
type Activation = Dur -> Bool
type Body meta a = Dur -> Term meta a

data Term meta a
  = a :%: Dur                                      -- ^ primitive
  | Term meta a :-: Term meta a                    -- ^ sequence
  | meta :$: (Term meta a)                         -- ^ auxiliary modifications
  | Let (Term meta a) (Term meta a -> Term meta a) -- ^ let (enables repetition)

instance (Eq a, Eq meta) => Eq (Term meta a) where
  (a :%: d)    == (a' :%: d')    = a == a' && d == d'
  (x :-: y)    == (x' :-: y')    = x == x' && y == y'
  (meta :$: t) == (meta' :$: t') = meta == meta' && t == t'
  (Let t _)    == (Let t' _)     = t == t'
  _            == _              = False

-- | Any metadata-carrying grammar term must be expanded to a stripped-down
-- grammar term with no metadata (i.e. `Term a ()`), possibly producing terms of
-- a different type `b`.
class Expand input a meta b | input a meta -> b where
  -- | Expand meta-information.
  expand :: input -> Term meta a -> IO (Term () b)

class ToMusic1 c => Interpret input b c | input b -> c where
  -- | Intepret terminal symbols as musical elements.
  interpret :: input -> Music b -> IO (Music c)

-- A `ToMusic1` datatype can be trivially interpreted.
instance ToMusic1 b => Interpret () b Note1 where
  interpret _ = return . toMusic1

-- | A term with no auxiliary wrappers can be trivially expanded.
instance Expand input a () a where
  expand = const return

type Grammarly input meta a b c =
  ( Eq a, Eq meta, ToMusic1 c
  , Expand input a meta b
  , Interpret input b c
  )

-- | Run a grammar with the given initial symbol.
runGrammar :: forall input meta a b c
            . Grammarly input meta a b c
           => Grammar meta a -- ^ the musical grammar
           -> Dur            -- ^ total duration
           -> input          -- ^ input configuration
           -> IO ( Music b -- ^ the abstract musical structure
                 , Music1  -- ^ the concrete interpretation as music
                 )
runGrammar (initial :|: rules) initT input = do
  rewritten    <- fixpoint step (initial :%: initT)
  expanded     <- expand input (unlet rewritten)
  let abstract = toMusic expanded
  concrete     <- toMusic1 <$> interpret input abstract
  return (abstract, concrete)
  where
    -- | Run one term of grammar rewriting.
    step :: Term meta a -> IO (Term meta a)
    step (Let x k)  = flip Let k <$> step x
    step (t :-: t') = (:-:) <$> step t <*> step t'
    step (m :$: t)  = (m :$:) <$> step t
    step (a :%: t)  = apply <$> pickRule a (filter isActive rules)
      where isActive ((a', _, p) :-> _) = a' == a && p t
            apply    (_ :-> rewrite)    = rewrite t

    -- | Convert to music (after expansion).
    toMusic :: Term () b -> Music b
    toMusic (a :%: t)  = Note' t a
    toMusic (t :-: t') = toMusic t :+: toMusic t'
    toMusic _          = error "toMusic: lets/aux after expansion"

    unlet :: Term meta a -> Term meta a
    unlet (Let x k)    = unlet (k x)
    unlet (t :-: t')   = unlet t :-: unlet t'
    unlet (meta :$: t) = meta :$: unlet t
    unlet t            = t

    -- | Converge to fixpoint with given initial value.
    fixpoint :: Eq x => (x -> IO x) -> x -> IO x
    fixpoint k l = do l' <- k l
                      if l == l' then return l
                                 else fixpoint k l'

    -- | Randomly pick a rule to rewrite given terminal.
    pickRule :: a -> [Rule meta a] -> IO (Rule meta a)
    pickRule a [] = return $ (a, 1, always) :-> (a :%:)
    pickRule _ rs = do
      let totalDouble = sum ((\((_, w, _) :-> _) -> w) <$> rs)
      index <- getStdRandom $ randomR (0, totalDouble)
      return $ pick' index rs
      where pick' :: Double -> [Rule meta a] -> Rule meta a
            pick' n (r@((_, w, _) :-> _):rest) =
              if n <= w then r else pick' (n-w) rest
            pick' _ _ = error "pick': empty list"

{- Grammar-specific operators. -}

-- | Rule which always activates.
always :: Activation
always = const True

-- | Conjunction of activation functions.
(/\) :: Activation -> Activation -> Activation
(f /\ g) x = f x && g x

-- | Disjunction of activation functions.
(\/) :: Activation -> Activation -> Activation
(f \/ g) x = f x || g x

-- | Rule with duration-independent body.
(|->) :: Head a -> Term meta a -> Rule meta a
a |-> b = a :-> const b
