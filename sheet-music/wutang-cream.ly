\version "2.24.3"

\header {
title = \markup { Wutang Clan - C.R.E.A.M. (C harmonic minor) }
author  = "Siddharth Bhat <siddu.druid@gmail.com>"
}

\score {
  \new Staff <<
    \new Voice \relative c' {
      \set midiInstrument = #"acoustic grand"
      \voiceOne
      \key c \major
      \time 4/4
      c d ees f4 g
      c8 \grace bes \grace c bes8 a4
      g4 g4 f ees4. d8 c4 c4 c
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
