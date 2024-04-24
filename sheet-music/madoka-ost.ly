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
      c4. d ees bes c d
      c4. d ees f ees f ees d
      \grace {d} ees c
      c d ees f g4. c
      g4. g f ees d4. \grace {d} ees c

      c d ees f g4. c
      g4. g fes d4.
      ees f g4.

      c d ees f g4. c
      g4. \grace {g} ees f
      ees4. ees f g
      d4. d ees c
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
