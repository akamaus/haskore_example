module MusicBlocks where

import Haskore.Basic.Pitch
import Medium.Temporal(Dur)
import Haskore.Melody(note)
import Haskore.Music(line)

import Data.List

data Chord a = Chord {chord_tones :: [a]} deriving Show
type RelChord = Chord Class

(#>) :: Chord Class -> Chord Class -> Chord Class
infixl 6 #>

Chord a #> Chord b = Chord $ a++b

chord_map f (Chord tones) = Chord (f tones)

up_arpeggio_in :: Octave -> Chord Class -> Chord T
up_arpeggio_in oct0 (Chord notes) = Chord pitches
 where pitches = snd $ mapAccumL getHigher (oct0-1, Bs) notes
       getHigher (prev_oct, prev_deg) deg =
           let acc = if classToInt prev_deg < classToInt deg
                      then (prev_oct, deg) else (prev_oct+1, deg)
           in (acc, acc)

down_arpeggio_in :: Octave -> Chord Class -> Chord T
down_arpeggio_in oct0 (Chord notes) = Chord pitches
 where pitches = snd $ mapAccumL getLower (oct0+1, C) $ reverse notes
       getLower (prev_oct, prev_deg) deg =
           let acc = if classToInt prev_deg > classToInt deg
                      then (prev_oct, deg) else (prev_oct-1, deg)
           in (acc, acc)

smoth_from :: Octave -> [Chord Class] -> Chord T
smoth_from oct0 cs = Chord $ snd $ mapAccumL smooth' (oct0, head tones) tones
    where tones = concatMap chord_tones cs

smooth' prev@(prev_oct, prev_deg) tone =
    let vars = [(prev_oct+1, tone), (prev_oct-1, tone), (prev_oct, tone)]
        dist_to_prev p = abs $ toInt prev - toInt p
        best = head $ sortBy (compareBy dist_to_prev) vars
    in (best, best)

compareBy f x y = compare (f x) (f y)

rev_chord = chord_map reverse


-- Builder helpers

invert :: Chord a -> Chord a
invert (Chord (n:ns)) = Chord $ ns ++ [n]

skip n (Chord ns) = Chord $ left ++ right
    where left = take (n-1) ns
          right = drop n ns

form_chord :: [Relative] -> Class -> Chord Class
form_chord form root = Chord $ map (snd . fromInt) $ zipWith (+) form (repeat rn)
    where rn = classToInt root

-- Chords
moll53 = form_chord [0, 3, 7]
dur53 = form_chord [0, 4, 7]
dim53 = form_chord [0, 3, 6]
aug53 = form_chord [0, 4, 8]

moll6 = invert . moll53
moll64 = invert . moll6
dur6 = invert . dur53
dur64 = invert . dur6

d7 = form_chord [0, 4, 7, 10]
d65 = invert . d7
d43 = invert . d65
d2 = invert . d43

-- Intervals
min2 = form_chord [0, 1]
maj2 = form_chord [0, 2]

min3 = form_chord [0, 3]
maj3 = form_chord [0, 4]

pure5 = form_chord [0, 7]
dim5 = form_chord [0, 6]

min6 = form_chord [0, 8]
maj6 = form_chord [0, 9]

min7 = form_chord [0, 10]

pure8 = form_chord [0, 12]
filled8 = form_chord [0, 7, 12]

-- Conversion to Hackage types
chord_to_notes len (Chord cs) = map (\p -> note p len ()) cs
