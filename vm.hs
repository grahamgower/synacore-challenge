{- An implementation of the https://challenge.synacor.com/ virtual machine -}
import Data.Bits
import Data.Char
import Data.Word
import System.Environment (getArgs, getProgName)
import System.IO
import Text.Printf (printf)

-- bytestring
import qualified Data.ByteString as B
-- primitive
import Control.Monad.Primitive (PrimState)
-- vector
import qualified Data.Vector.Unboxed.Mutable as MU


{- Read an instruction argument. If the argument points at a register,
   read the value in the register. -}
getOpArg
  :: MU.MVector (PrimState IO) Word16
  -> MU.MVector (PrimState IO) Word16
  -> Int
  -> IO Word16
getOpArg mem reg i = do
  x <- MU.read mem i
  if (x .&. 0x8000 == 0) then
    return x
  else
    let r = x .&. 0x7fff
    in
      if r < 8 then do
        MU.read reg (fromIntegral r)
      else
        error $ printf "0x%04x: invalid argument 0x%04x" i x

{- Read an instruction argument, which must point at a register,
   and return the register number 0 to 7. -}
getReg :: MU.MVector (PrimState IO) Word16 -> Int -> IO Word16
getReg mem i = do
  x <- MU.read mem i
  if x == (x .&. 0x8007) then
    return (x .&. 0x7)
  else
    error $ printf "0x%04x: invalid register argument 0x%04x" i x

{- Turn the VM crank. -}
vm
  :: MU.MVector (PrimState IO) Word16
  -> MU.MVector (PrimState IO) Word16
  -> [Word16]
  -> Int
  -> IO ()
vm mem reg stack ip =
  let arg'a = getOpArg mem reg (ip + 1)
      arg'b = getOpArg mem reg (ip + 2)
      arg'c = getOpArg mem reg (ip + 3)
      reg'a = getReg mem (ip + 1)
  in do
    op <- MU.read mem ip
    --printf "0x%04x: 0x%04x\n" ip op
    case op of
      -- halt
      0 -> return ()
      -- set
      1 -> do
        a <- reg'a
        b <- arg'b
        MU.write reg (fromIntegral a) b
        vm mem reg stack (ip + 3)
      -- push
      2 -> do
        a <- arg'a
        let stack' = a : stack
        vm mem reg stack' (ip + 2)
      -- pop
      3 ->
        case stack of
          (x:stack') -> do
            a <- reg'a
            MU.write reg (fromIntegral a) x
            vm mem reg stack' (ip + 2)
          [] -> error $ printf "0x%04x: attempt to pop empty stack" ip
      -- eq
      4 -> do
        a <- reg'a
        b <- arg'b
        c <- arg'c
        let a' = if b == c then 1 else 0
        MU.write reg (fromIntegral a) a'
        vm mem reg stack (ip + 4)
      -- gt
      5 -> do
        a <- reg'a
        b <- arg'b
        c <- arg'c
        let a' = if b > c then 1 else 0
        MU.write reg (fromIntegral a) a'
        vm mem reg stack (ip + 4)
      -- jmp
      6 -> do
        a <- arg'a
        let ip' = fromIntegral a
        vm mem reg stack ip'
      -- jt
      7 -> do
        a <- arg'a
        b <- arg'b
        let ip' = if a /= 0 then fromIntegral b else (ip + 3)
        vm mem reg stack ip'
      -- jf
      8 -> do
        a <- arg'a
        b <- arg'b
        let ip' = if a == 0 then fromIntegral b else (ip + 3)
        vm mem reg stack ip'
      -- add
      9 -> do
        a <- reg'a
        b <- arg'b
        c <- arg'c
        let a' = (b + c) `mod` 32768
        MU.write reg (fromIntegral a) a'
        vm mem reg stack (ip + 4)
      -- mult
      10 -> do
        a <- reg'a
        b <- arg'b
        c <- arg'c
        let b' = fromIntegral b :: Word32
        let c' = fromIntegral c :: Word32
        let a' = fromIntegral $ (b' * c') `mod` 32768
        MU.write reg (fromIntegral a) a'
        vm mem reg stack (ip + 4)
      -- mod
      11 -> do
        a <- reg'a
        b <- arg'b
        c <- arg'c
        let a' = b `mod` c
        MU.write reg (fromIntegral a) a'
        vm mem reg stack (ip + 4)
      -- and
      12 -> do
        a <- reg'a
        b <- arg'b
        c <- arg'c
        let a' = b .&. c
        MU.write reg (fromIntegral a) a'
        vm mem reg stack (ip + 4)
      -- or
      13 -> do
        a <- reg'a
        b <- arg'b
        c <- arg'c
        let a' = b .|. c
        MU.write reg (fromIntegral a) a'
        vm mem reg stack (ip + 4)
      -- not
      14 -> do
        a <- reg'a
        b <- arg'b
        let a' = (complement b) .&. 0x7fff
        MU.write reg (fromIntegral a) a'
        vm mem reg stack (ip + 3)
      -- rmem
      15 -> do
        a <- reg'a
        b <- arg'b
        a' <- MU.read mem (fromIntegral b)
        MU.write reg (fromIntegral a) a'
        vm mem reg stack (ip + 3)
      -- wmem
      16 -> do
        a <- arg'a
        b <- arg'b
        MU.write mem (fromIntegral a) b
        vm mem reg stack (ip + 3)
      -- call
      17 -> do
        a <- arg'a
        let stack' = fromIntegral (ip + 2) : stack
        let ip' = fromIntegral a
        vm mem reg stack' ip'
      -- ret
      18 -> do
        case stack of
          (x:stack') -> do
            let ip' = fromIntegral x
            vm mem reg stack' ip'
          [] ->
            return ()
      --  out
      19 -> do
        a <- arg'a
        let ch = chr $ fromIntegral (a .&. 0xff)
        putChar ch
        vm mem reg stack (ip + 2)
      -- in
      20 -> do
        -- TODO: handle EOF?
        System.IO.hFlush System.IO.stdout
        a <- reg'a
        ch <- getChar >>= (return . fromIntegral . ord)
        MU.write reg (fromIntegral a) ch
        vm mem reg stack (ip + 2)
      -- noop
      21 -> vm mem reg stack (ip + 1)
      _ -> error $ printf "0x%04x: unknown instruction: %d" ip op


{- Pack the given bytes into a 32k vector of words (aka memory). -}
packMem :: B.ByteString -> IO (MU.MVector (PrimState IO) Word16)
packMem bytes =
  let num_bytes = B.length bytes
      mem_len = 32768 -- in 16bit words
      get_word16 j =
        case (bytes B.!? (2 * j), bytes B.!? (2 * j + 1)) of
          (Just a, Just b) ->
            let a' = fromIntegral a :: Word16
                b' = fromIntegral b :: Word16
            in b' `shiftL` 8 .|. a'
          _ -> 0
  in
    if (num_bytes `mod` 2) /= 0 || num_bytes >= (2 * mem_len) then
      error "invalid program: length is not mod 2, or is longer than 2^16 bytes"
    else
      MU.generate mem_len get_word16


main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      bytes <- B.readFile filename
      mem <- packMem bytes
      reg <- MU.replicate 8 (0 :: Word16)
      let stack = [] :: [Word16]
      let ip = 0
      vm mem reg stack ip
    _ -> do
      progName <- getProgName
      putStrLn $ "usage: " <> progName <> " challenge.bin"
