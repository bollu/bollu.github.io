\version "2.24.3"

\header {
title = \markup { C minor i ii V Jazz Chord Voicing}
author  = "Siddharth Bhat <siddu.druid@gmail.com>"
}

melody = {
  \override Score.SpacingSpanner.spacing-increment = #5
  < c' d' f' aes'>1
  < d' f' g' b'>1
  < ees' g' b' c''>1

}

<<
  \new NoteNames { \melody }
  \melody
>>
