{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}

module Assembler where

import Asm
import Free

import Control.Arrow ( (>>>) )
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Binary.Put as P
import Data.Bits
import qualified Data.ByteString.Lazy as BS
import Data.Word

type AsmMem = Asm Address
type AsmMemF = Free AsmMem

data AssemblerError
    = UnsupportedOpcode (AsmMem ())
    | InvalidOpcode

data AssemblerState
    = AssemblerState
        { _codeOffset :: Word64
        }

initialState = AssemblerState { _codeOffset = 0 }

newtype AssemblerT m a
    = AssemblerT
        { runAssemblerT :: StateT AssemblerState (ExceptT AssemblerError m) a
        }
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadState AssemblerState
        , MonadError AssemblerError
        )

instance MonadTrans AssemblerT where
    lift = AssemblerT . lift . lift

type AssemblerM = AssemblerT P.PutM
type Assembler = AssemblerM ()

-- | Outputs arbitrary data with no concern for the assembler state.
unsafePut :: P.Put -> Assembler
unsafePut = lift

-- | Emits bytes to the output, adjusting the assembler state to track the
-- number of written bytes.
emit :: P.Put -> Assembler
emit p = do
    let l = fromIntegral . BS.length . P.runPut $ p
    modify $ \s -> s { _codeOffset = _codeOffset s + l }
    unsafePut p

codeOffset :: AssemblerM Word64
codeOffset = gets _codeOffset

assembleArg :: AsmMem Assembler -> Assembler
assembleArg a = case a of
    Ret m -> do
        emit $ do
            P.putWord8 0xc3
        m
    Mov (R reg) (I i) m -> do
        emit $ do
            binEncode $ rexW rexPrefix
            P.putWord8 (0xb8 + index reg)
            P.putWord64le (fromIntegral i)
        m
    Mov (R r1) (R r2) m -> do
        emit $ do
            binEncode $ rexW rexPrefix
            P.putWord8 0x8b
            binEncode $ registerDirect r1 r2
        m
    Mov (R r1) (IR r2) m -> do
        emit $ do
            binEncode $ rexW rexPrefix
            P.putWord8 0x8b
            binEncode $ zeroIndirect r1 r2
        m
    Mov (IR r1) (R r2) m -> do
        emit $ do
            binEncode $ rexW rexPrefix
            P.putWord8 0x89
            binEncode $ zeroIndirect r1 r2
        m
    Inc (R reg) m -> do
        emit $ do
            binEncode $ rexW rexPrefix
            P.putWord8 0xff
            binEncode $ opcodeExtension RegisterDirect 0 reg
        m
    Inc (IR reg) m -> do
        emit $ do
            binEncode $ rexW rexPrefix
            P.putWord8 0xff
            binEncode $ opcodeExtension ZeroIndirect 0 reg
        m
    Dec (R reg) m -> do
        emit $ do
            binEncode $ rexW rexPrefix
            P.putWord8 0xff
            binEncode $ opcodeExtension RegisterDirect 1 reg
        m
    Dec (IR reg) m -> do
        emit $ do
            binEncode $ rexW rexPrefix
            P.putWord8 0xff
            binEncode $ opcodeExtension ZeroIndirect 1 reg
        m
    Loop (A addr) m -> do
        off <- codeOffset
        emit $ do
            P.putWord8 0xe2
            P.putWord8 (fromIntegral $ addr - off + 2)
        m
    Label k -> do
        off <- codeOffset
        k off
    Syscall m -> do
        emit $ do
            P.putWord8 0x0f
            P.putWord8 0x05
        m
    Push (R reg) m -> do
        emit $ P.putWord8 (0x50 + index reg)
        m
    Pop (R reg) m -> do
        emit $ P.putWord8 (0x58 + index reg)
        m
    _ -> do
        throwError $ UnsupportedOpcode (a $> ())

assemble :: AsmMemF () -> Either AssemblerError BS.ByteString
assemble
    = uncurry ($>)
    . P.runPutM
    . runExceptT
    . flip evalStateT initialState
    . runAssemblerT
    . foldFM assembleArg

class BinaryEncodable a where
    binEncode :: a -> P.Put

data RexPrefix
    = RexPrefix
        { _rexW :: !Bool
        , _rexR :: !Bool
        , _rexX :: !Bool
        , _rexB :: !Bool
        }
    deriving (Eq, Show)

-- | An REX prefix with no flags set.
rexPrefix :: RexPrefix
rexPrefix
    = RexPrefix
        { _rexW = False
        , _rexR = False
        , _rexX = False
        , _rexB = False
        }

-- | Enable the W flag of the REX prefix.
rexW :: RexPrefix -> RexPrefix
rexW p = p { _rexW = True }

-- | Enable the R flag of the REX prefix.
rexR :: RexPrefix -> RexPrefix
rexR p = p { _rexR = True }

-- | Enable the X flag of the REX prefix.
rexX :: RexPrefix -> RexPrefix
rexX p = p { _rexX = True }

-- | Enable the B flag of the REX prefix.
rexB :: RexPrefix -> RexPrefix
rexB p = p { _rexB = True }

boolBit :: Num a => Bool -> a
boolBit True = 1
boolBit False = 0

instance BinaryEncodable RexPrefix where
    binEncode (RexPrefix (boolBit -> w) (boolBit -> r) (boolBit -> x) (boolBit -> b))
        = P.putWord8 byte where
            byte
                =   (1 `shift` 6)
                .|. (w `shift` 3)
                .|. (r `shift` 2)
                .|. (x `shift` 1)
                .|. b

data DispType
    = ZeroIndirect
    | OneIndirect
    | FourIndirect
    | RegisterDirect
    deriving (Eq, Show)

dispVal :: Num a => DispType -> a
dispVal d = case d of
    ZeroIndirect   -> 0 -- b00
    OneIndirect    -> 1 -- b01
    FourIndirect   -> 2 -- b10
    RegisterDirect -> 3 -- b11

-- | A ModR/M byte.
data ModRm
    = ModRmRr DispType Register Register
    -- ^ A ModR/M byte with a displacement type and two registers. The
    -- interpretation of the ModR/M byte depends on the instruction it is
    -- involved in. The first register is the /reg/ field of the byte, and
    -- the second is the R/M field.
    | ModRmOpEx DispType Word8 Register
    -- ^ A ModR/M byte without a displacement, but with an opcode extension
    -- field and a single "Register" representing the R/M field.
    -- Only the three least-significant bits of the opcode extension are kept!
    deriving (Eq, Show)

registerDirect :: Register -> Register -> ModRm
registerDirect reg rm = ModRmRr RegisterDirect reg rm

zeroIndirect :: Register -> Register -> ModRm
zeroIndirect reg rm = ModRmRr ZeroIndirect reg rm

oneIndirect :: Register -> Register -> ModRm
oneIndirect reg rm = ModRmRr OneIndirect reg rm

fourIndirect :: Register -> Register -> ModRm
fourIndirect reg rm = ModRmRr FourIndirect reg rm

opcodeExtension :: DispType -> Word8 -> Register -> ModRm
opcodeExtension = ModRmOpEx

instance BinaryEncodable ModRm where
    binEncode (ModRmRr (dispVal -> d) (index -> reg) (index -> rm))
        = P.putWord8
        $   (d `shift` 6)
        .|. (reg `shift` 3)
        .|. rm
    binEncode (ModRmOpEx (dispVal -> d) ex (index -> rm))
        = P.putWord8
        $   (d `shift` 6)
        .|. (7 .&. ex `shift` 3)
        .|. rm
