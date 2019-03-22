module Answers.Bootcamp.Hymn where

import Prelude
import Data.Vec (Vec, vec3, fill)
import Data.Typelevel.Num (D3, D8)
import Partial.Unsafe (unsafePartial)


hymn :: Vec D3 (Vec D8 String)
hymn = vec3 verse1 verse2 verse3
  where
    verse1 = fill $ unsafePartial $ \i -> case i of
      0 -> "From the Halls of Montezuma"
      1 -> "To the shores of Tripoli;"
      2 -> "We fight our country's battles"
      3 -> "In the air, on land, and sea;"
      4 -> "First to fight for right and freedom"
      5 -> "And to keep our honor clean;"
      6 -> "We are proud to claim the title"
      7 -> "Of United States Marine."
    verse2 = fill $ unsafePartial $ \i -> case i of
      0 -> "Our flag's unfurled to every breeze"
      1 -> "From dawn to setting sun;"
      2 -> "We have fought in every clime and place"
      3 -> "Where we could take a gun;"
      4 -> "In the snow of far-off Northern lands"
      5 -> "And in sunny tropic scenes;"
      6 -> "You will find us always on the job"
      7 -> "The United States Marines."
    verse3 = fill $ unsafePartial $ \i -> case i of
      0 -> "Here's health to you and to our Corps"
      1 -> "Which we are proud to serve;"
      2 -> "In many a strife we've fought for life"
      3 -> "And never lost our nerve;"
      4 -> "If the Army and the Navy"
      5 -> "Ever look on Heaven's scenes,"
      6 -> "They will find the streets are guarded"
      7 -> "By United States Marines."
