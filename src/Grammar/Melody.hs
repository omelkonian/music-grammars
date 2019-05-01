module Grammar.Melody
       ( MelodyConfig (..), defMelodyConfig
       , melody
       ) where

import Control.Arrow (first)

import Grammar.Types
import Grammar.Utilities
import Music ( Music
             , AbstractChord, AbstractScale, Octave, SemiChord, Pitch
             , Dur, PitchClass(..), Interval(..)
             , (^.), (^^^), (#), (<~), (~>), (=|)
             , wn, hn, qn, en, sn
             , allScales, allOctaves )

-- | Melodic (non)-terminal symbols.
data NT = MQ -- Meta-rhythm
        | Q  -- Rhythm non-terminal
        | MN -- Meta-note
        | N  -- Note non-terminal
        | HT -- any of [CT, L, AT]
        | CT -- chord tone
        | L  -- color tone
        | AT -- approach tone
        | ST -- scale tone
        | R  -- rest
        deriving (Eq, Show)

-- | Grammar for melodic lines based on the paper:
-- "A Grammatical Approach to Automatic Improvisation" by Robert M. Keller.
melody :: Grammar () NT
melody = MQ :|:
  [ -- Rhythm { expand MQ(*) to multiple Q(wn), Q(hn) and Q(qn) }
    (MQ, 1, (== 0))      |-> R:%:0
  , (MQ, 1, (== qn))     |-> Q:%:qn
  , (MQ, 1, (== hn))     |-> Q:%:hn
  , (MQ, 1, (== (hn^.))) |-> Q:%:hn :-: Q:%:qn
  , (MQ, 25, (> (hn^.)))  :-> \t -> Q:%:hn :-: MQ:%:(t - hn)
  , (MQ, 75, (> wn))      :-> \t -> Q:%:wn :-: MQ:%:(t - wn)

    -- Melody { expand Qs to notes }
  , (Q, 52, (== wn)) |-> Q:%:hn :-: MN:%:qn :-: MN:%:qn
  , (Q, 47, (== wn)) |-> MN:%:qn :-: Q:%:hn :-: MN:%:qn
  , (Q,  1, (== wn)) |-> MN:%:en :-: N:%:qn :-: N:%:qn :-: N:%:qn :-: MN:%:en
  , (Q, 60, (== hn)) |-> Let (MN:%:qn) (\x -> x :-: x)
  , (Q, 16, (== hn)) |-> HT:%:(qn^.) :-: N:%:en
  , (Q, 12, (== hn)) |-> MN:%:en :-: N:%:qn :-: MN:%:en
  , (Q,  6, (== hn)) |-> N:%:hn
  , (Q,  6, (== hn)) |-> HT:%:(qn^^^) :-: HT:%:(qn^^^) :-: HT:%:(qn^^^)

  , (Q, 1, (== qn)) |-> CT:%:qn

  , (MN, 1, (== wn)) |-> Let (MN:%:qn) (\x -> x :-: x :-: x :-: x)

  , (MN, 72, (== qn)) |-> Let (MN:%:en) (\x -> x :-: x)
  , (MN, 22, (== qn)) |-> N:%:qn
  , (MN,  5, (== qn)) |-> Let (HT:%:(en^^^)) (\x -> x :-: x :-: x)
  , (MN,  1, (== qn)) |-> Let (HT:%:(en^^^)) (\x -> x :-: x :-: AT:%:(en^^^))

  , (MN, 99, (== en)) |-> N:%:en
  , (MN,  1, (== en)) |-> HT:%:sn :-: AT:%:sn

  , (N, 1, (== hn)) |-> CT:%:hn

  , (N, 50, (== qn)) |-> CT:%:qn
  , (N, 50, (== qn)) |-> ST:%:qn
  , (N, 45, (== qn)) |-> R:%:qn
  , (N, 20, (== qn)) |-> L:%:qn
  , (N,  1, (== qn)) |-> AT:%:qn

  , (N, 40, (== en)) |-> CT:%:en
  , (N, 40, (== en)) |-> ST:%:en
  , (N, 20, (== en)) |-> L:%:en
  , (N, 20, (== en)) |-> R:%:en
  , (N,  1, (== en)) |-> AT:%:en
  ]

-- | Configuration for melody.
data MelodyConfig = MelodyConfig
  { scales         :: [(Double, AbstractScale)]
  , octaves        :: [(Double, Octave)]
  }

defMelodyConfig :: MelodyConfig
defMelodyConfig    = MelodyConfig
  { scales         = equally allScales
  , octaves        = equally allOctaves
  }

-- | Produce a concrete improvisation out of a melodic structure.
instance Interpret (MelodyConfig, Music SemiChord) NT Pitch where
  interpret (mCfg, chs) nts =
    fromListM <$> go Nothing [] (synchronize (toList chs) (toList nts))
   where
    go :: Maybe Pitch -> [Dur] -> ListMusic (SemiChord, NT) -> IO (ListMusicM Pitch)
    go _ _ [] = return []
    go prevP approach (((ch, nt), t):rest) =
      case nt of
        HT -> do
          nt' <- choose [(5, CT), (3, AT), (2, L)]
          go prevP approach (((ch, nt'), t):rest)
        AT -> if null rest then return [] else go prevP (approach ++ [t]) rest
        _  -> do m <- interpretNT prevP approach ch nt t
                 (++) <$> pure m <*> go (fst $ last m) [] rest

    interpretNT :: Maybe Pitch -- ^ previous pitch
                -> [Dur]       -- ^ approach tones
                -> SemiChord   -- ^ harmonic context
                -> NT          -- ^ current tone characteristic
                -> Dur         -- ^ current duration
                -> IO (ListMusicM Pitch)
    interpretNT prevP approach ch nt t =
      case nt of
        R  -> do return $ (,) Nothing <$> (t : approach)
        CT -> do mkPitch prevP approach t ch
        ST ->
          let scales' = [(w, sc) | (w, sc) <- scales mCfg, all (`elem` sc) (toIntervals ch)]
          in if null scales'
               then interpretNT prevP approach ch CT t
               else do sc <- choose scales'
                       mkPitch prevP approach t (head ch =| sc)
        L ->
          let colors = colorTones ch
          in if null colors
               then interpretNT prevP approach ch CT t
               else mkPitch prevP approach t colors
        _ -> error $ "intrepret: incomplete grammar rewrite " ++ show nt ++ " <| " ++ show t

    mkPitch :: Maybe Pitch -> [Dur] -> Dur -> [PitchClass] -> IO (ListMusicM Pitch)
    mkPitch prevP approach t pcs =
      (fst <$> chooseWith setWeight ps) >>= approachPitch approach prevP t
      where
          ps = [(pc#oct, w) | pc <- pcs, (w, oct) <- normally $ octaves mCfg]
          setWeight (p', w') =
            w' * (1.0 - (fromIntegral (pitchDistanceM prevP p') / 12.0))

    approachPitch :: [Dur] -> Maybe Pitch -> Dur -> Pitch -> IO (ListMusicM Pitch)
    approachPitch approach prevP t p = reverse <$> oneOf [move dir | dir <- directions]
      where
        move dir = first Just <$> zip (iterate (`dir` Mi2) p) (t : approach)
        directions = case prevP of
          Just p' -> if p' > p then [(<~)] else [(~>)]
          Nothing -> [(~>), (<~)]

    -- | Synchronize the harmonic background with the melodic foreground.
    synchronize :: ListMusic SemiChord -> ListMusic NT -> ListMusic (SemiChord, NT)
    synchronize [] _  = []
    synchronize _  [] = []
    synchronize ((ch, t):back) front =
      let (ps', front') = takeTime front t
      in  [((ch, p'), t') | (p', t') <- ps' ] ++ synchronize back front'

    takeTime :: ListMusic NT -> Dur -> (ListMusic NT, ListMusic NT)
    takeTime ntz d
      | d <= 0 = ([], ntz)
      | otherwise = case ntz of
          [] -> ([], [])
          (nt@(_, d'):ntz') ->
            let (ntz'', rest) = takeTime ntz' (d - d')
            in  (nt:ntz'', rest)

    -- | Extracts the color tones of a chord.
    colorTones :: SemiChord -> [PitchClass]
    colorTones (p:ps) = filter (\p' -> distancePc p p' `elem` colorIntervals) ps
      where colorIntervals = [M3, Mi3, Mi7, M7, Mi9, M9, M13, Mi13]
    colorTones [] = []

    toIntervals :: SemiChord -> AbstractChord
    toIntervals ch = P1 : (uncurry distancePc <$> zip ch (tail ch))
