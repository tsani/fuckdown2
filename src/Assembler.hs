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

assembleArg :: AsmMem Assembler -> Assembler
assembleArg a = case a of
    Ret m -> do
        emit $ do
            P.putWord8 0xc3
        m
    Mov (R reg) (I i) m -> do
        emit $ do
            undefined
        m
    _ -> do
        throwError $ UnsupportedOpcode (a $> ())

assemble :: AsmMemF () -> Assembler
assemble = foldFM assembleArg

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
