\version "2.24.3"

\header {
title = \markup { Madoka Magica OST (C Aeolian / Natural Minor)}
author  = "Siddharth Bhat <bollu@pixel-druid.com>"
}

\score {
  \new Staff <<
    \new Voice \relative c' {
      \set midiInstrument = #"acoustic grand"
      \voiceOne
      c4. d ees bes c d
      c4. d ees f4. ees d4.
      \grace {d} ees c4.
      c d ees f g4. c
      g4  f4. ees4. d4.
      \grace {d} ees c4.

      c d ees f g4. c,
      g4  f4. ees4. d4.
      ees f g4.

      c d ees f g4. c
      g4. \grace {g} aes f4.
      ees4. \grace{ees} f g4.
      d4. \grace{d} ees c4.
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
