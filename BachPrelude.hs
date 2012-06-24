module BachPrelude where

import MusicBlocks
import Snippet

import Haskore.Music(articulation, Articulation(..))

import Haskore.Basic.Pitch
import Haskore.Basic.Duration
import Haskore.Music(line, changeTempo)

import Data.List

prelude_skeleton :: [(Octave, RelChord, RelChord)]
prelude_skeleton = [
 -- part1
 (1, maj3 C,  dur64 C),
 (1, maj2 C,  moll64 D),
 (0, min3 B, skip 2 $ d7 G),
 (1, maj3 C,  dur64 C),
 --part2
 (1, maj3 C,  filled8 A),
 (1, maj2 C,  dur6 D),
 (0, min3 B, filled8 G),
 (0, min2 B, dur6 C),
 (0, min3 A, dur6 C),

 (0, pure5 D, skip 3 $ d7 D),(0, maj3 G,  dur64 G),

 (0, min3 G, invert $ dim53 Cs), (0, maj3 F,  filled8 D),
 (0, min3 F, invert $ dim53 B),  (0, min3 E,  filled8 C),

 (0, min2 E, dur6 F), (0, min3 D, dur6 F),
 (-1, pure5 G, skip 3 $ d7 G), (0, maj3 C, dur64 C),
 -- bar 20
 (0, pure5 C,  skip 4 $ d2 C),
 (-1, pure8 F, moll53 A), (-1, dim5 Fs, dim53 A),
 (-1, maj6 Af, Chord [B,C,D]),

 (-1, min7 G, dur53 G), (-1, maj6 G, dur64 C), (-1, pure5 G, Chord [F,C,F]), (-1, pure5 G, skip 3 $ d7 G),
 {-!-}(-1, min6 G, invert $ dim53 Fs), (-1, maj6 G, Chord [G, C, G]), (-1, pure5 G, Chord [G,C,F]), (-1, pure5 G, skip 3 $ d7 G), (-1, pure8 C, invert $ dim53 E)
 ]

-- basic pattern of prelude
fill_skeleton (octave, left_hand, right_hand) =
    let tones = chord_in octave $ left_hand #> right_hand
        right_part = chord_map (drop 2) tones
    in concat $ replicate 2 $ [tones, right_part] --repeat top three notes of a chord and repeat whole construct

prelude = changeTempo 6 $ line $ map (articulation Pedal . line . map (line . chord_to_arp qn) . fill_skeleton) $ prelude_skeleton