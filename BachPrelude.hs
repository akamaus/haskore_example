module BachPrelude where

import MusicBlocks
import Snippet

import Haskore.Melody

import Haskore.Basic.Pitch
import Haskore.Basic.Duration
import Haskore.Music(line, changeTempo)

import Data.List

prelude_skeleton :: [(Octave, RelChord, RelChord)]
prelude_skeleton = [
 -- part1
 (1, dur3 C,  dur64 C),
 (1, dur2 C,  moll64 D),
 (0, moll3 B, skip2 $ d7 G),
 (1, dur3 C,  dur64 C),
 --part2
 (1, dur3 C,  filled8 A),
 (1, dur2 C,  dur6 D),
 (0, moll3 B, filled8 G),
 (0, moll2 B, dur6 C),
 (0, moll3 A, dur6 C),

 (0, pure5 D, skip3 $ d7 D),(0, dur3 G,  dur6 G),
 (0, pure5 D, skip3 $ d7 D),(0, dur3 G,  dur6 G),

 (0, moll3 G, dim53 Cs), (0, dur3 F,  filled8 D),
 (0, moll3 F, dim53 B),  (0, moll3 E,  filled8 C),

 (0, moll2 E, dur6 F), (0, moll3 D, dur6 F),
 (-1, pure5 G, skip3 $ d7 G), (-1, dur3 C,  dur64 C)

 ]


-- basic pattern of prelude
fill_skeleton (octave, left_hand, right_hand) =
    let tones = chord_in octave $ left_hand #> right_hand
        right_part = chord_map (drop 2) tones
    in concat $ replicate 2 $ [tones, right_part] --repeat top three notes of a chord and repeat whole construct

prelude = changeTempo 8 $ line $ concatMap (chord_to_arp qn) $ concatMap fill_skeleton $ prelude_skeleton