module Snippet where

import Haskore.Music.GeneralMIDI as MidiMusic
import Haskore.Interface.MIDI.Render as Render
import Haskore.Melody
import Haskore.Music

import System.Cmd

render_to f m = Render.fileFromGeneralMIDIMusic f song where
  song = MidiMusic.fromMelodyNullAttr MidiMusic.AcousticGrandPiano m

play_music music = do
  let f = "test.mid"
      note = c 1 0.125 ()
  render_to f music
  rawSystem "timidity" [f]