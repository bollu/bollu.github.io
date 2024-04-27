\version "2.24.3"

\header {
title = \markup { Bossa Nova Drum Pattern }
author  = "Siddharth Bhat <bollu@pixel-druid.com>"
}

upper = \relative c'' {
  \voiceOne
  % beats on 1 (+3) 4 (+3) 7 (+4) 11 (+3) 14. draw it out, it's a pentagon.
  
  % 1 and (1 2) 
  c8 r8
  % 2 and (3 4)
  r8 c8 
  % 3 and (5 6) 
  r8 r8
  % 4 and (7 8) 
  c8 r8
  % 5 and (9 10) 
  r8 r8
  % 6 and (11 12)
  c8 r8
  % 7 and (13 14)
  r8 c8
  % 8 and (15 16)
  r8 r8
}

lower = \relative c' {
  \voiceTwo
  % 1 and 
  c8~c8
  % 2 and 
  r8 g'8
  % 3 and 
  g8~g8
  % 4 and 
  r8 c,8
  % 5 and 
  c8~c8
  % 6 and 
  r8 g'8
  % 7 and 
  g8~g8
  % 8 and
  r8 c,8 
}


\score {

  \new Staff <<
    \clef treble
    \key c \major
    \time 2/8
    \new Voice = "upper" \upper
    \new Voice = "lower" \lower
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
