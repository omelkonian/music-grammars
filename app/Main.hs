module Main where

import Control.Monad (when, forM_)
import System.Directory

import Grammar
import Music

main :: IO ()
main =
  forM_ [sonata, romanianElegy, byzantineDance, orientalAlgebras] $ \run ->
    forM_ [1..10] $ \n ->
      cd ("output/" ++ show n) run
  where
    cd :: FilePath -> IO a -> IO a
    cd fpath c = createDirectoryIfMissing True fpath
              >> withCurrentDirectory fpath c


generate :: FilePath -> (Dur, Dur) -> HarmonyConfig -> MelodyConfig -> IO ()
generate f (t, tmp) hCfg mCfg = do
  when (t < 4 * wn) $
    fail "integrate: requested duration should be at least 4 bars of music"
  (absHarm, harm) <- runGrammar harmony t hCfg
  (_,       mel)  <- runGrammar melody t (mCfg, absHarm)
  (_,       rhy)  <- runGrammar rhythm t ()

  forM_ [(harm, 'h'), (mel, 'm'), (rhy, 'r')] $ \(m, c) ->
    writeToMidiFile (f ++ ('-':c:".mid")) tmp m
  -- playDev 4 $ setDynamic PPP (harm :=: mel) -- :=: rhy

-- Sonata in E Minor.
sonata :: IO ()
sonata
  = generate "sonata" (16 * wn, 7%10)
    HarmonyConfig
      { basePc    = E
      , baseOct   = 4
      , baseScale = minor
      , chords    = equally [mi, maj, dim] }
    MelodyConfig
       { scales  = equally [ionian, harmonicMinor]
       , octaves = [(5, 4), (20, 5), (10, 6)] }

-- Romanian Elegy for Piano & Cello.
romanianElegy :: IO ()
romanianElegy
  = generate "romanian" (16 * wn, 1)
    HarmonyConfig
        { basePc    = C
        , baseOct   = 4
        , baseScale = romanian
        , chords    = equally [mi, maj, aug, dim, m7, m7b5] }
    MelodyConfig
        { scales  = equally allScales
        , octaves = [(20, 3), (15, 4), (10, 5)] }

-- Byzantine dance for Harpsichord.
byzantineDance :: IO ()
byzantineDance
  = generate "byzantine" (16 * wn, 7%4)
    HarmonyConfig
        { basePc    = Fs
        , baseOct   = 4
        , baseScale = byzantine
        , chords    = equally allChords
        }
    MelodyConfig
        { scales  = equally allScales
        , octaves = [(1, 3), (20, 4), (15, 5), (1, 6)] }

-- Oriental Algebras for Metalophone, Sitar & Tablas.
orientalAlgebras :: IO ()
orientalAlgebras
  = generate "oriental" (16 * wn, 6%5)
    HarmonyConfig
        { basePc    = A
        , baseOct   = 3
        , baseScale = arabian
        , chords    = equally allChords }
    MelodyConfig
        { scales  = equally allScales
        , octaves = [(20, 4), (15, 5), (5, 6)] }

harmonicMinorSong :: IO ()
harmonicMinorSong
  = generate "harmonicMinor" (16 * wn, 3%4)
    HarmonyConfig
        { basePc    = B
        , baseOct   = 3
        , baseScale = harmonicMinor
        , chords    = equally [maj, mi, aug, dim, maj7, m7, dim7, d7] }
    MelodyConfig
        { scales  = equally [harmonicMinor]
        , octaves = equally [4..6] }

bebopMajorSong :: IO ()
bebopMajorSong
  = generate "bebopMajor" (16 * wn, 7%4)
    HarmonyConfig
        { basePc    = As
        , baseOct   = 4
        , baseScale = bebopMajor
        , chords    = equally [maj, mi, maj6, maj7, m7, d7, m7b5, dim7, d7b9, d9, m9, maj9] }
    MelodyConfig
        { scales  = equally [bebopMajor]
        , octaves = equally [5..7] }

makam :: IO ()
makam
  = generate "makam" (16 * wn, 7%4)
    HarmonyConfig
        { basePc    = D
        , baseOct   = 4
        , baseScale = phrygianDominant
        , chords    = equally [ maj, mi, maj6, maj7, m7, d7, m7b5
                              , d7b5, d7s5, d7b9, d7s9, d7b5b9, d7b5s9, d7s5b9, d7s5s9
                              ] }
    MelodyConfig
        { scales  = equally [phrygianDominant]
        , octaves = equally [4..7] }

lydianSong :: IO ()
lydianSong
  = generate "lydian" (16 * wn, 3%4)
    HarmonyConfig
        { basePc    = Fs
        , baseOct   = 4
        , baseScale = lydian
        , chords    = equally [ maj, mi, maj6, maj7, mmaj7, m7, d7, m7b5, aug, maj9, m9, d9 ] }
    MelodyConfig
        { scales  = equally [lydian]
        , octaves = equally [4..6] }
