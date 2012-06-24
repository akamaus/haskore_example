module BachPrelude where

import MusicBlocks
import Snippet

import Haskore.Music(articulation, Articulation(..))

import Haskore.Basic.Pitch
import Haskore.Basic.Duration
import Haskore.Music(line, chord, changeTempo)

import Data.List

prelude_part1 :: [(Octave, RelChord, RelChord)]
prelude_part1 = [
 (1, maj3 C,  dur64 C),
 (1, maj2 C,  moll64 D),
 (0, min3 B, skip 2 $ d7 G),
 (1, maj3 C,  dur64 C),

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

 (0, pure5 C,  skip 4 $ d2 C),
 (-1, pure8 F, moll53 A), (-1, dim5 Fs, dim53 A),
 (-1, maj6 Af, Chord [B,C,D]),

 (-1, min7 G, dur53 G), (-1, maj6 G, dur64 C), (-1, pure5 G, Chord [F,C,F]), (-1, pure5 G, skip 3 $ d7 G),
 (-1, min6 G, invert $ dim53 Fs), (-1, maj6 G, Chord [G, C, G]), (-1, pure5 G, Chord [G,C,F]), (-1, pure5 G, skip 3 $ d7 G), (-1, pure8 C, invert $ dim53 E)
 ]

type PinnedChord = (Octave, Chord Class)
prelude_part2 :: [(PinnedChord, PinnedChord, [PinnedChord])]
prelude_part2 =
    [((-1, pure8 C), (0, dur53 F), [(1, dur6 F), (1,dur53 F), (0, moll53 D), (0, min3 D)]),
     ((-1, pure8 C), (1, dur53 G), [(2, dim53 B), (2, dur53 G), (1, maj6 D), (1, Chord [D,E,F])])]

prelude_part3 = [(-1, pure8 C), (1, dur6 C)]

-- basic pattern of prelude
proc_part1 (octave, left_hand, right_hand) =
    let tones = up_arpeggio_in octave $ left_hand #> right_hand
        right_part = chord_map (drop 2) tones
    in concat $ replicate 2 $ [tones, right_part] --repeat top three notes of a chord and repeat whole construct

proc_part2 (left, right_up, left_downs) = up left : up right_up : map down left_downs
 where up = uncurry up_arpeggio_in
       down = uncurry down_arpeggio_in

-- whole piece
prelude = let
    p1 = line . map (articulation Pedal . line . map (line . chord_to_notes qn) . proc_part1) $ prelude_part1
    p2 = line . concatMap (chord_to_notes qn) . concatMap proc_part2 $ prelude_part2
    p3 = chord . concatMap (chord_to_notes wn . uncurry up_arpeggio_in) $ prelude_part3
 in changeTempo 6 $ line [p1,p2,p3]