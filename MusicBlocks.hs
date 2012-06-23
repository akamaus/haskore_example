module MusicBlocks where

import Haskore.Basic.Pitch
import Medium.Temporal(Dur)
import Haskore.Melody(note)
import Haskore.Music(line)

import Data.List

data Chord a = Chord [a] deriving Show
type RelChord = Chord Class

(#>) :: Chord Class -> Chord Class -> Chord Class
infixl 6 #>

Chord a #> Chord b = Chord $ a++b

chord_map f (Chord tones) = Chord (f tones)

chord_in :: Octave -> Chord Class -> Chord T
chord_in oct0 (Chord notes) = Chord pitches
 where pitches = snd $ mapAccumL getHigher (oct0-1, Bs) notes
       getHigher (prev_oct, prev_deg) deg =
           let acc = if classToInt prev_deg < classToInt deg
                      then (prev_oct, deg) else (prev_oct+1, deg)
           in (acc, acc)

-- Builder helpers

invert :: Chord a -> Chord a
invert (Chord (n:ns)) = Chord $ ns ++ [n]

skip2 (Chord (n1:n2:ns)) = Chord (n1:ns)
skip3 (Chord (n1:n2:n3:ns)) = Chord (n1:n2:ns)

form_chord :: [Relative] -> Class -> Chord Class
form_chord form root = Chord $ map (snd . fromInt) $ zipWith (+) form (repeat rn)
    where rn = classToInt root

-- Chords
moll53 = form_chord [0, 3, 7]
dur53 = form_chord [0, 4, 7]
dim53 = form_chord [0, 3, 6]

moll6 = invert . moll53
moll64 = invert . invert . moll53
dur6 = invert . dur53
dur64 = invert . invert . dur53

d7 = form_chord [0, 4, 7, 10]

-- Intervals
moll2 = form_chord [0, 1]
dur2 = form_chord [0, 2]

moll3 = form_chord [0, 3]
dur3 = form_chord [0, 4]

pure5 = form_chord [0, 7]
filled8 = form_chord [0, 7, 12]

-- Conversion to Hackage types
chord_to_arp len (Chord cs) = map (\p -> note p len ()) cs
