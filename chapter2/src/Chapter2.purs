module Chapter2 where

import Math
import Debug.Trace

diagonal w h = sqrt (w * w + h * h)

circleArea r = pi * r * r

main = print (diagonal 3 4)
