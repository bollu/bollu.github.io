\version "2.24.3"

\header {
title = \markup { Fur Elise (C blues scale) }
author  = "Siddharth Bhat <bollu@pixel-druid.com>"
}

\score {
  \new Staff <<
    \new Voice \relative c' {
      \set midiInstrument = #"acoustic grand"
      \voiceOne
      \key c \major
      \time 4/4
      g'' fis g fis g
      d f ees c
      f, aes b d
      g, b d es
      c g' fis g fis g
      d f ees c
      f, aes c d
      g ees d c
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

\paper {
  indent = 0\mm
  line-width = 110\mm
  oddHeaderMarkup = ""
  evenHeaderMarkup = ""
  oddFooterMarkup = ""
  evenFooterMarkup = ""
}
