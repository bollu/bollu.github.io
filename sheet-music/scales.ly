\version "2.24.3"

\header {
title = \markup {C Minor Scales}
author  = "Siddharth Bhat <bollu@pixel-druid.com>"
}

\score {
  \new Staff <<
    \new Voice \relative c' {
      \set midiInstrument = #"acoustic grand"
      \voiceOne
      \time 8/4 
      \textMark "C minor natural / aeolian mode."
      c d ees f g aes bes c \break |
      \textMark "C minor phrygian"
      \time 8/4  c, \once \override NoteHead.color = "#26C6DA" des ees f g aes bes c \break |
      \textMark "C minor dorian"
      c d ees f g aes bes c \break |
      \textMark "C minor harmonic"
      \time 8/4  c, d ees f g  \once \override NoteHead.color = "#26C6DA"  aes b c \break |
      \textMark "C blues"
      \time 7/4  c, ees f fis g bes c \break |
      \textMark "C Hirajoshi"
      \time 6/4  c, cis f fis bes c \break |
      \textMark "C Pentatonic"
       c, ees f g bes c \break |
    }

  >>


  \layout { }
  \midi {
    \context {
      \Staff
      \remove "Staff_performer"
    }
    \context {
      \Voice
      \consists "Staff_performer"
    }
    \tempo 2 = 120
  }
}
