{- Find a solution to the teleporter problem. -}

import Data.Word

{-
call 0x178b  ;; r0=4, r1=1
0x178b: Jt      0x8000 0x1793
0x178e: Add     0x8000 0x8001 0x0001
0x1792: Ret

0x1793: Jt      0x8001 0x17a0
0x1796: Add     0x8000 0x8000 0x7fff
0x179a: Set     0x8001 0x8007
0x179d: Call    0x178b
0x179f: Ret

0x17a0: Push    0x8000
0x17a2: Add     0x8001 0x8001 0x7fff
0x17a6: Call    0x178b
0x17a8: Set     0x8001 0x8000
0x17ab: Pop     0x8000
0x17ad: Add     0x8000 0x8000 0x7fff
0x17b1: Call    0x178b
0x17b3: Ret
-}
f :: Word16 -> Word16 -> Word16 -> Word16
f 0 r1 _ = r1 + 1
f 1 r1 r7 = r7 + r1 + 1
f 2 r1 r7 = r7 + (r7 + 1) * (r1 + 1)
f 3 r1 r7 = r7 + (sum $ map ((r7 + 1)^) [2..(r1+2)])
f r0 0 r7 = f (r0 - 1) r7 r7
f r0 r1 r7 = f (r0 - 1) (f r0 (r1 - 1) r7) r7

main :: IO ()
main = do
  let f41 = [(j, f 4 1 j) | j<-[0..]]
  print $ fst . head $ filter (\a -> snd a == 6) f41
