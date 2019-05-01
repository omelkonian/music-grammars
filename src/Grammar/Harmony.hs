{-# LANGUAGE FlexibleContexts      #-}
module Grammar.Harmony
       ( HarmonyConfig (..), defHarmonyConfig
       , harmony
       ) where

import Grammar.Types
import Grammar.Utilities
import Music ( AbstractChord, Chord, SemiChord, AbstractScale, SemiScale
             , Dur, Octave, PitchClass(..), Interval(..)
             , invertChord, (~>), (<~), (=|), (//)
             , wn, hn, qn
             , major, allChords
             )

-- | Terminal symbol that represents scale degrees.
data Degree =
  -- terminals
  I | II | III | IV | V | VI | VII
  -- non-terminals
  | Piece | TR | DR | SR | TS | DS | SS
  deriving (Eq, Show, Enum, Bounded)

-- | Auxiliary wrapper for modulating keys.
newtype Modulation = Modulation Interval deriving Eq

-- | Grammar for tonal harmony, based on the paper:
-- "Towards a Generative Syntax of Tonal Harmony" by Martin Rohrmeier.
harmony :: Grammar Modulation Degree
harmony = Piece :|:
  [ -- Phrase level
    (Piece, 1, always) :-> \t ->
      foldr1 (:-:) $ replicate (t // (4 * wn)) $ TR:%:(4 * wn)

    -- Functional level: Expansion
  , (TR, 1, (> wn)) :-> \t -> TR:%:t/2 :-: DR:%:t/2
  , (TR, 1, always) :-> \t -> DR:%:t/2 :-: TS:%:t/2
  , (DR, 1, always) :-> \t -> SR:%:t/2 :-: DS:%:t/2
  ] ++
  [ (x, 1, (> wn)) :-> \t -> Let (x:%:t/2) (\y -> y :-: y)
  | x <- [TR, SR, DR]
  ] ++
  [
    (TR, 1, always) :-> (TS :%:)
  , (DR, 1, always) :-> (DS :%:)
  , (SR, 1, always) :-> (SS :%:)

    -- Functional level: Modulation
  , (DS, 1, (>= qn)) :-> \t -> Modulation P5 :$: DS:%:t
  , (SS, 1, (>= qn)) :-> \t -> Modulation P4 :$: SS:%:t

    -- Scale-degree level: Secondary dominants
  ] ++
  [ (x, 1, (>= hn)) :-> \t -> Let (x:%:t/2) (\y -> (Modulation P5 :$: y) :-: y)
  | x <- [TS, DS, SS]
  ] ++
  [ -- Scale-degree level: Functional-Scale interface
    (TS, 1, (>= wn)) :-> \t -> I:%:t/2 :-: IV:%:t/4 :-: I:%:t/4
  , (TS, 1, always) :-> (I :%:)
  , (SS, 1, always) :-> (IV :%:)
  , (DS, 1, always) :-> (V :%:)
  , (DS, 1, always) :-> (VI :%:)
  ]

-- | Configuration for harmony.
data HarmonyConfig = HarmonyConfig
  { basePc    :: PitchClass
  , baseOct   :: Octave
  , baseScale :: AbstractScale
  , chords    :: [(Double, AbstractChord)]
  }

defHarmonyConfig :: HarmonyConfig
defHarmonyConfig = HarmonyConfig
  { basePc    = C
  , baseOct   = 4
  , baseScale = major
  , chords    = equally allChords
  }

-- | Expands modulations and intreprets degrees to chords.
instance Expand HarmonyConfig Degree Modulation SemiChord where
  expand conf (m :-: m') = (:-:) <$> expand conf m <*> expand conf m'
  expand conf (Modulation itv :$: t) =
    expand (conf {basePc = basePc conf ~> itv}) t
  expand conf (a :%: t) = do
    ch <- conf `interpretDegree` a
    return $ ch :%: t
    where
      -- | Interpret a degree as a 'SemiChord' on a given harmonic context.
      interpretDegree :: HarmonyConfig -> Degree -> IO SemiChord
      interpretDegree config degree = choose options
        where tonic = basePc config =| baseScale config :: SemiScale
              tone = tonic !! fromEnum degree
              options = [ (w, ch)
                        | (w, chordType) <- chords config
                        , let ch = tone =| chordType
                        , all (`elem` tonic) ch
                        ]
  expand _ _ = error "Expand: let-expressions exist"

-- | Produce concrete chords out of a harmonic structure.
instance Interpret HarmonyConfig SemiChord Chord where
  interpret hCfg m' = do
    vl <- foldl f (pure [(c0, t)]) ms
    return $ fromList vl
    where
      c0            = toBaseChord c
      ((c, t) : ms) = toList m'

      f :: IO [(Chord, Dur)] -> (SemiChord, Dur) -> IO [(Chord, Dur)]
      f cs' (sc, d) = do
        cs <- cs'
        c' <- smoothTransition c0 (fst $ last cs) sc
        return $ cs ++ [(c', d)]

      -- | Get a basic voicing of a chord in a given octave.
      toBaseChord :: SemiChord -> Chord
      toBaseChord = fmap (\pc -> (pc, baseOct hCfg))

      -- | Get all inversions of +-1 octave.
      allInversions :: SemiChord -> [Chord]
      allInversions c' =
        invs (initC ~> P8) ++ invs initC ++ invs (initC <~ P8)
        where
          initC = toBaseChord c'
          n = length c'
          invs ch = take n $ iterate invertChord ch

      -- | Smooth voice-leading from one chord to another (i.e. minimal pitch distance).
      smoothTransition :: Chord -> Chord -> SemiChord -> IO Chord
      smoothTransition initC' prevC curC =
        chooseWith (setWeight initC' prevC) (allInversions curC)

      -- | Set probability weight based on (inverse) pitch distance.
      setWeight :: Chord -> Chord -> Chord -> Double
      setWeight initC' prevC c' =
        1.0 / fromIntegral (2 * chordDistance initC' c' + chordDistance prevC c')
