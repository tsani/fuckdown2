{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}

module Assembler where

import Asm
import Free

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State hiding ( put )
import qualified Data.Binary.Put as P
import Data.Bits
import qualified Data.ByteString.Lazy as BS
import qualified Data.IntMap as M
import Data.Word

type LabelTable addr = M.IntMap addr

data LabelError = UndefinedLabel

data AssemblerError label addr
    = UnsupportedOpcode (Asm label addr ())
    | InvalidOpcode
    | UnassignedLabel
    | LabelError LabelError

data AssemblerState addr
    = AssemblerState
        { _codeOffset :: addr
        , _labelMap :: LabelTable (Maybe addr)
        }

-- | Assembler labels in the code.
newtype Label
    = LabelVal
        { unLabel :: Int
        }
    deriving (Eq, Show)

-- | An initial assembler state whose code offset is zero and whose label map
-- contains one key assigned to the start address.
initialState :: Num a => AssemblerState a
initialState
    = AssemblerState
        { _codeOffset = 0
        , _labelMap = M.singleton 0 (Just 0)
        }

newtype AssemblerT label addr m a
    = AssemblerT
        { runAssemblerT
            :: StateT (AssemblerState addr)
                      (ExceptT (AssemblerError label addr)
                               m)
               a
        }
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadState (AssemblerState addr)
        , MonadError (AssemblerError label addr)
        )

type R addr a = (ReaderT (LabelTable addr) (ExceptT LabelError P.PutM) a)
type AssemblerM addr = AssemblerT Label addr Identity
type Assembler addr a = AssemblerM addr (R addr a)

put :: P.Put -> R addr ()
put = lift . lift

offset :: Num addr => addr -> AssemblerM addr ()
offset a = modify $ \s -> s { _codeOffset = _codeOffset s + a }

-- | Assigns an arbitrary address to a label.
unsafeSetLabel :: Label -> Maybe addr -> AssemblerM addr ()
unsafeSetLabel (LabelVal l) addr
    = modify $ \s -> s { _labelMap = M.insert l addr (_labelMap s) }

-- | Gets the current offset in the assembled code.
codeOffset :: AssemblerM addr addr
codeOffset = gets _codeOffset

-- | Gets the label map of the assembler.
labelMap :: AssemblerM addr (LabelTable (Maybe addr))
labelMap = gets _labelMap

-- | Looks up the code offset of a label.
lookupLabel :: Label -> R addr addr
lookupLabel (LabelVal i) = do
    addr <- asks (i `M.lookup`)
    case addr of
        Nothing -> do
            throwError UndefinedLabel
        Just a -> do
            return a

emit :: (Monad m, Monad n) => m (n b) -> n a -> m (n b)
emit m a = do
    r <- m
    return $ do
        _ <- a
        r

emitPut :: Monad m
        => m (ReaderT (LabelTable addr) (ExceptT LabelError P.PutM) b)
        -> P.Put
        -> m (ReaderT (LabelTable addr) (ExceptT LabelError P.PutM) b)
emitPut m a = emit m (put a)

assembleArg :: Integral addr
            => Asm Label addr (Assembler addr ())
            -> Assembler addr ()
assembleArg a = case a of
    Ret m -> do
        offset 1
        emitPut m $ do
            P.putWord8 0xc3
    Mov (R reg) (I i) m -> do
        offset 10
        emitPut m $ do
            binEncode $ rexW rexPrefix
            P.putWord8 (0xb8 + index reg)
            P.putWord64le (fromIntegral i)
    Mov (R r1) (R r2) m -> do
        offset 3
        emitPut m $ do
            binEncode $ rexW rexPrefix
            P.putWord8 0x8b
            binEncode $ registerDirect r1 r2
    Mov (R r1) (IR r2) m -> do
        offset 3
        emitPut m $ do
            binEncode $ rexW rexPrefix
            P.putWord8 0x8b
            binEncode $ zeroIndirect r1 r2
    Mov (IR r1) (R r2) m -> do
        offset 3
        emitPut m $ do
            binEncode $ rexW rexPrefix
            P.putWord8 0x89
            binEncode $ zeroIndirect r1 r2
    Inc (R reg) m -> do
        offset 3
        emitPut m $ do
            binEncode $ rexW rexPrefix
            P.putWord8 0xff
            binEncode $ opcodeExtension RegisterDirect 0 reg
    Inc (IR reg) m -> do
        offset 3
        emitPut m $ do
            binEncode $ rexW rexPrefix
            P.putWord8 0xff
            binEncode $ opcodeExtension ZeroIndirect 0 reg
    Dec (R reg) m -> do
        offset 3
        emitPut m $ do
            binEncode $ rexW rexPrefix
            P.putWord8 0xff
            binEncode $ opcodeExtension RegisterDirect 1 reg
    Dec (IR reg) m -> do
        offset 3
        emitPut m $ do
            binEncode $ rexW rexPrefix
            P.putWord8 0xff
            binEncode $ opcodeExtension ZeroIndirect 1 reg
    Loop (L l) m -> do
        offset 2
        off <- codeOffset
        emit m $ do
            addr <- lookupLabel l
            put $ do
                P.putWord8 0xe2
                P.putWord8 (fromIntegral $ addr - off)
    NewLabel k -> do
        m <- labelMap
        let (i, _) = M.findMax m
        let l = LabelVal (i + 1)
        unsafeSetLabel l Nothing
        k l
    SetLabel l m -> do
        o <- codeOffset
        unsafeSetLabel l (Just o)
        m
    Here k -> do
        o <- codeOffset
        k o
    Syscall m -> do
        offset 2
        emitPut m $ do
            P.putWord8 0x0f
            P.putWord8 0x05
    Push (R reg) m -> do
        offset 1
        emitPut m $ P.putWord8 (0x50 + index reg)
    Pop (R reg) m -> do
        offset 1
        emitPut m $ P.putWord8 (0x58 + index reg)
    Int (I 3) m -> do
        offset 1
        emitPut m $ P.putWord8 0xcc
    Int (I v) m -> do
        offset 2
        emitPut m $ do
            P.putWord8 0xcd
            P.putWord8 (fromIntegral v)
    Cmp (R Rax) (I i) m -> do
        offset 10
        emitPut m $ do
            binEncode $ rexW rexPrefix
            P.putWord8 0x3d
            P.putWord64le (fromIntegral i)
    Cmp (IR reg) (I i) m -> do
        offset 7
        emitPut m $ do
            binEncode $ rexW rexPrefix
            P.putWord8 0x81
            binEncode $ opcodeExtension ZeroIndirect 7 reg
            P.putWord32le (fromIntegral i)
    Cmp (R r1) (R r2) m -> do
        offset 3
        emitPut m $ do
            binEncode $ rexW rexPrefix
            P.putWord8 0x3b
            binEncode $ registerDirect r1 r2
    Cmp (R r1) (IR r2) m -> do
        offset 3
        emitPut m $ do
            binEncode $ rexW rexPrefix
            P.putWord8 0x3b
            binEncode $ zeroIndirect r1 r2
    Cmp (IR r1) (R r2) m -> do
        offset 3
        emitPut m $ do
            binEncode $ rexW rexPrefix
            P.putWord8 0x39
            binEncode $ zeroIndirect r2 r1
    Je (L l) m -> do
        offset 2
        off <- codeOffset
        emit m $ do
            addr <- lookupLabel l
            put $ do
                P.putWord8 0x74
                P.putWord8 (fromIntegral $ addr - off)
    Jne (L l) m -> do
        offset 2
        off <- codeOffset
        emit m $ do
            addr <- lookupLabel l
            put $ do
                P.putWord8 0x75
                P.putWord8 (fromIntegral $ addr - off)
    _ -> do
        throwError $ UnsupportedOpcode (a $> ())

assemble :: AsmF Label Word64 ()
         -> Either (AssemblerError Label Word64) BS.ByteString
assemble asm = do
    let e = runIdentity
          . runExceptT
          . flip runStateT initialState
          . runAssemblerT
          . foldFM assembleArg
          $ (asm $> return ())

    (r, s) <- e

    let m = sequence (_labelMap s)

    labels <- case m of
        Nothing -> Left UnassignedLabel
        Just ls -> return ls

    let e' = uncurry ($>)
           . P.runPutM
           . runExceptT
           . flip runReaderT labels
           $ r

    case e' of
        Left l -> Left $ LabelError l
        Right x -> return x

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
        .|. ((7 .&. ex) `shift` 3)
        .|. rm
