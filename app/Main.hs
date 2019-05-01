{-# LANGUAGE ImplicitParams      #-}
module Main where

import Control.Monad (when)

import Grammar
import Music

main :: IO ()
main = asymmetry

generate :: FilePath -> Dur -> HarmonyConfig -> MelodyConfig -> IO ()
generate f t hCfg mCfg = do
  when (t < 4 * wn) $
    fail "integrate: requested duration should be at least 4 bars of music"
  (absHarm, harm) <- runGrammar harmony t hCfg
  (_,       mel)  <- runGrammar melody t (mCfg, absHarm)
  (_,       rhy)  <- runGrammar rhythm t ()

  writeToMidiFile (f ++ "-h.mid") harm
  writeToMidiFile (f ++ "-m.mid") mel
  writeToMidiFile (f ++ "-r.mid") rhy
  playDev 4 $ harm :=: mel -- :=: rhy

-- Sonata in E Minor.
sonata :: IO ()
sonata
  = generate "sonata" (120 * wn)
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
  = generate "romanian" (12 * wn)
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
  = generate "byzantine" (8 * wn)
    HarmonyConfig
        { basePc    = Fs
        , baseOct   = 4
        , baseScale = byzantine
        , chords    = equally allChords
        }
    MelodyConfig
        { scales  = equally allScales
        , octaves = [(1, 3), (20, 4), (15, 5), (1, 6)]
        }

-- Oriental Algebras for Metalophone, Sitar & Tablas.
orientalAlgebras :: IO ()
orientalAlgebras
  = generate "oriental" (12 * wn)
    HarmonyConfig
        { basePc    = A
        , baseOct   = 3
        , baseScale = arabian
        , chords    = equally allChords }
    MelodyConfig
        { scales         = equally allScales
        , octaves        = [(20, 4), (15, 5), (5, 6)] }

-- Assymetry.
asymmetry :: IO ()
asymmetry
  = generate "assymetry" (48 * wn)
    HarmonyConfig
        { basePc    = Fs
        , baseOct   = 3
        , baseScale = halfDiminished
        , chords    = equally allChords }
    MelodyConfig
        { scales  = equally allScales
        , octaves = equally [4..7] }

-- Symmetry.
symmetry :: IO ()
symmetry
  = generate "symmetry" (48 * wn)
    HarmonyConfig
        { basePc    = Gs
        , baseOct   = 4
        , baseScale = wholeTone
        , chords    = equally [aug, dim, m7b5, sus4, d7sus4, maj7, m7, mmaj7] }
    MelodyConfig
        { scales  = equally [wholeTone, harmonicMinor, altered]
        , octaves = equally [5..7] }
