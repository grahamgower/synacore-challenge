{- An implementation of the https://challenge.synacor.com/ virtual machine -}
import Control.Concurrent
import Data.Bits
import Data.Char
import Data.Word
import System.Environment (getArgs, getProgName)
import System.IO
import Text.Printf (printf, hPrintf)

-- bytestring
import qualified Data.ByteString as B
-- primitive
import Control.Monad.Primitive (PrimState)
-- vector
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU


data Opcode
  = Halt
  | Set
  | Push
  | Pop
  | Eq
  | Gt
  | Jmp
  | Jt
  | Jf
  | Add
  | Mult
  | Mod
  | And
  | Or
  | Not
  | Rmem
  | Wmem
  | Call
  | Ret
  | Out
  | In
  | Noop
  deriving (Show, Read)


decodeInstr :: Word16 -> (Opcode, Int)
decodeInstr instr =
  case instr of
    0 -> (Halt, 0)
    1 -> (Set, 2)
    2 -> (Push, 1)
    3 -> (Pop, 1)
    4 -> (Eq, 3)
    5 -> (Gt, 3)
    6 -> (Jmp, 1)
    7 -> (Jt, 2)
    8 -> (Jf, 2)
    9 -> (Add, 3)
    10 -> (Mult, 3)
    11 -> (Mod, 3)
    12 -> (And, 3)
    13 -> (Or, 3)
    14 -> (Not, 2)
    15 -> (Rmem, 2)
    16 -> (Wmem, 2)
    17 -> (Call, 1)
    18 -> (Ret, 0)
    19 -> (Out, 1)
    20 -> (In, 1)
    21 -> (Noop, 0)
    _ -> error $ printf "invalid instruction: 0x%04x" instr

{- Print disassembled instructions from 'ip' until Ret or Halt is encountered. -}
disassemble :: MU.MVector (PrimState IO) Word16 -> Int -> IO ()
disassemble mem ip = do
  instr <- MU.read mem ip
  let (op, nargs) = decodeInstr instr
  printf "0x%04x: %s" ip (show op)
  case nargs of
    0 -> do
      printf "\n"
    1 -> do
      a <- MU.read mem (ip + 1)
      printf "\t0x%04x\n" a
    2 -> do
      a <- MU.read mem (ip + 1)
      b <- MU.read mem (ip + 2)
      printf "\t0x%04x 0x%04x\n" a b
    3 -> do
      a <- MU.read mem (ip + 1)
      b <- MU.read mem (ip + 2)
      c <- MU.read mem (ip + 3)
      printf "\t0x%04x 0x%04x 0x%04x\n" a b c
    _ -> undefined
  case op of
    Halt -> return ()
    Ret -> return ()
    _ -> disassemble mem (ip + nargs + 1)


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
  :: Chan (Maybe Char)
  -> MVar (
    MU.MVector (PrimState IO) Word16,
    MU.MVector (PrimState IO) Word16,
    [Word16],
    Int,
    Maybe Opcode
  )
  -> MU.MVector (PrimState IO) Word16
  -> MU.MVector (PrimState IO) Word16
  -> [Word16]
  -> Int
  -> Maybe Opcode
  -> IO ()
vm in_q state mem reg stack ip trace =
  let arg'a = getOpArg mem reg (ip + 1)
      arg'b = getOpArg mem reg (ip + 2)
      arg'c = getOpArg mem reg (ip + 3)
      reg'a = getReg mem (ip + 1)
  in do
    instr <- MU.read mem ip
    let (op, nargs) = decodeInstr instr
    let ip' = ip + nargs + 1
    case op of
      Halt -> return ()

      Set -> do
        a <- reg'a
        b <- arg'b
        MU.write reg (fromIntegral a) b
        vm in_q state mem reg stack ip' trace

      Push -> do
        a <- arg'a
        let stack' = a : stack
        vm in_q state mem reg stack' ip' trace

      Pop ->
        case stack of
          (x:stack') -> do
            a <- reg'a
            MU.write reg (fromIntegral a) x
            vm in_q state mem reg stack' ip' trace
          [] -> error $ printf "0x%04x: attempt to pop empty stack" ip

      Eq -> do
        a <- reg'a
        b <- arg'b
        c <- arg'c
        let a' = if b == c then 1 else 0
        MU.write reg (fromIntegral a) a'
        vm in_q state mem reg stack ip' trace

      Gt -> do
        a <- reg'a
        b <- arg'b
        c <- arg'c
        let a' = if b > c then 1 else 0
        MU.write reg (fromIntegral a) a'
        vm in_q state mem reg stack ip' trace

      Jmp -> do
        a <- arg'a
        let ip'' = fromIntegral a
        vm in_q state mem reg stack ip'' trace

      Jt -> do
        a <- arg'a
        b <- arg'b
        let ip'' = if a /= 0 then fromIntegral b else ip'
        vm in_q state mem reg stack ip'' trace

      Jf -> do
        a <- arg'a
        b <- arg'b
        let ip'' = if a == 0 then fromIntegral b else ip'
        vm in_q state mem reg stack ip'' trace

      Add -> do
        a <- reg'a
        b <- arg'b
        c <- arg'c
        let a' = (b + c) `mod` 32768
        MU.write reg (fromIntegral a) a'
        vm in_q state mem reg stack ip' trace

      Mult -> do
        a <- reg'a
        b <- arg'b
        c <- arg'c
        let b' = fromIntegral b :: Word32
        let c' = fromIntegral c :: Word32
        let a' = fromIntegral $ (b' * c') `mod` 32768
        MU.write reg (fromIntegral a) a'
        vm in_q state mem reg stack ip' trace

      Mod -> do
        a <- reg'a
        b <- arg'b
        c <- arg'c
        let a' = b `mod` c
        MU.write reg (fromIntegral a) a'
        vm in_q state mem reg stack ip' trace

      And -> do
        a <- reg'a
        b <- arg'b
        c <- arg'c
        let a' = b .&. c
        MU.write reg (fromIntegral a) a'
        vm in_q state mem reg stack ip' trace

      Or -> do
        a <- reg'a
        b <- arg'b
        c <- arg'c
        let a' = b .|. c
        MU.write reg (fromIntegral a) a'
        vm in_q state mem reg stack ip' trace

      Not -> do
        a <- reg'a
        b <- arg'b
        let a' = (complement b) .&. 0x7fff
        MU.write reg (fromIntegral a) a'
        vm in_q state mem reg stack ip' trace

      Rmem -> do
        a <- reg'a
        b <- arg'b
        a' <- MU.read mem (fromIntegral b)
        MU.write reg (fromIntegral a) a'
        vm in_q state mem reg stack ip' trace

      Wmem -> do
        a <- arg'a
        b <- arg'b
        MU.write mem (fromIntegral a) b
        case trace of
          Just Wmem ->
            hPrintf stderr "0x%04x: Wmem 0x%04x 0x%04x\n" ip a b
          _ ->
            return ()
        vm in_q state mem reg stack ip' trace

      Call -> do
        a <- arg'a
        let stack' = fromIntegral ip' : stack
        let ip'' = fromIntegral a
        case trace of
          Just Call ->
            hPrintf stderr "0x%04x: Call 0x%04x\n" ip ip''
          _ ->
            return ()
        vm in_q state mem reg stack' ip'' trace

      Ret -> do
        case stack of
          (x:stack') -> do
            let ip'' = fromIntegral x
            vm in_q state mem reg stack' ip'' trace
          [] ->
            return ()

      Out -> do
        a <- arg'a
        let ch = chr $ fromIntegral (a .&. 0xff)
        putChar ch
        vm in_q state mem reg stack ip' trace

      In -> do
        putMVar state (mem, reg, stack, ip, trace)
        x <- readChan in_q
        (_, _, _, _, trace') <- takeMVar state
        case x of
          Just ch -> do
            let char = fromIntegral $ ord ch
            a <- reg'a
            MU.write reg (fromIntegral a) char
            vm in_q state mem reg stack ip' trace'
          _ ->
            -- EOF, we're done.
            return ()

      Noop -> vm in_q state mem reg stack ip' trace

debugger
  :: MVar (
    MU.MVector (PrimState IO) Word16,
    MU.MVector (PrimState IO) Word16,
    [Word16],
    Int,
    Maybe Opcode
  )
  -> IO ()
debugger state = do
    (mem, reg, stack, ip, trace) <- takeMVar state
    trace' <- debug_loop mem reg stack ip trace
    putMVar state (mem, reg, stack, ip, trace')
    return ()
    where
    debug_loop
      :: MU.MVector (PrimState IO) Word16
      -> MU.MVector (PrimState IO) Word16
      -> [Word16]
      -> Int
      -> Maybe Opcode
      -> IO (Maybe Opcode)
    debug_loop mem reg stack ip trace = do
      putStr "debug> "
      System.IO.hFlush System.IO.stdout
      eof <- System.IO.isEOF
      if eof then do
        -- Exit the debugger console.
        putStrLn ""
        return trace
      else do
        line <- getLine
        let cmd = words line
        case cmd of
          ["reg"] -> do
            -- Print registers
            reg_list <- U.freeze reg >>= (return . U.toList)
            putStrLn $ unwords $ map (printf "0x%04x" :: Word16 -> String) reg_list
            debug_loop mem reg stack ip trace
          ["wreg", index, value] -> do
            -- Write value to the register with the given index.
            let i = read index
            let v = read value
            MU.write reg i v
            debug_loop mem reg stack ip trace
          ["stack"] -> do
            -- Print the stack.
            putStrLn $ unwords $ map (printf "0x%04x" :: Word16 -> String) stack
            debug_loop mem reg stack ip trace
          ["ip"] -> do
            -- Print instruction pointer.
            printf "0x%04x\n" ip
            debug_loop mem reg stack ip trace
          ("mem":indices) ->
            -- Print memory at given indices.
            let
                print_mem :: [String] -> IO ()
                print_mem [] = do printf "\n"
                print_mem (idx:idxs) = do
                  let i = read idx :: Int
                  x <- MU.read mem i
                  printf " 0x%04x" x
                  print_mem idxs
            in do
              print_mem indices
              debug_loop mem reg stack ip trace
          ["wmem", index, value] -> do
            -- Write value to the memory at the given index.
            let i = read index
            let v = read value
            MU.write mem i v
            debug_loop mem reg stack ip trace
          ["trace"] -> do
            -- Print the current trace state.
            print trace
            debug_loop mem reg stack ip trace
          ["trace", op] -> do
            trace' <- case read op of
              Call -> do return $ Just Call
              Wmem -> do return $ Just Wmem
              _ -> do
                putStrLn $ "don't know how to trace `" <> op <> "'"
                return Nothing
            -- Turn on tracing. Call/Ret instructions will be printed.
            debug_loop mem reg stack ip trace'
          ["dis", offset] -> do
            -- Disassembly from the given offset until the next Ret or Halt.
            disassemble mem (read offset)
            debug_loop mem reg stack ip trace
          ["printx", index, key] ->
            -- Print xor-encoded string.
            let i = read index
                k = read key :: Word16
                printx' _ 0 = do return ()
                printx' j n = do
                  x <- MU.read mem j
                  putChar $ (chr . fromIntegral) (x `xor` k)
                  printx' (j + 1) (n - 1)
            in do
              len <- MU.read mem i
              printx' (i + 1) len
              debug_loop mem reg stack ip trace
          ["quit"] -> do
            -- Exit the debugger console.
            return trace
          [] ->
            debug_loop mem reg stack ip trace
          _ -> do
            putStrLn $ "unknown command `" <> line <> "'"
            debug_loop mem reg stack ip trace


monitor
  :: Chan (Maybe Char)
  -> MVar (
    MU.MVector (PrimState IO) Word16,
    MU.MVector (PrimState IO) Word16,
    [Word16],
    Int,
    Maybe Opcode
  )
  -> IO ()
monitor q state = do
  System.IO.hFlush System.IO.stdout
  eof <- System.IO.isEOF
  if eof then
    -- Signal to exit.
    writeChan q Nothing
  else do
    line <- getLine
    if line == "debug" then
      -- Enter the debugger.
      debugger state
    else do
      writeList2Chan q (fmap Just (line <> "\n"))
    monitor q state


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
      in_q <- newChan
      state <- newEmptyMVar
      _ <- forkIO (monitor in_q state)
      bytes <- B.readFile filename
      mem <- packMem bytes
      reg <- MU.replicate 8 (0 :: Word16)
      let stack = [] :: [Word16]
      let ip = 0
      let trace = Nothing
      vm in_q state mem reg stack ip trace
    _ -> do
      progName <- getProgName
      putStrLn $ "usage: " <> progName <> " challenge.bin"
