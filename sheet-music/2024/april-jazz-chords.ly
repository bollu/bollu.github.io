\version "2.24.3"

\header {
title = "C minor II V I Jazz Chord Voicing"
author  = "Siddharth Bhat <siddu.druid@gmail.com>"
}

{
  \override Score.SpacingSpanner.spacing-increment = #5
  < c' d' f' aes'>1
  < d' f' g' b'>1
  < ees' g' b' c''>1

}

\addlyrics {
  \markup{C D F A}
  \markup{D F G B}
  \markup{E\flat G B C}
}


