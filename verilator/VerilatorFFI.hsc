{-# LANGUAGE RecordWildCards, ForeignFunctionInterface #-}
module VerilatorFFI where

import Prelude
import Clash.Prelude

import Data.Word
import Data.Int
import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Alloc

data INPUT = INPUT {
    iRESET :: Bit
    , iBTN_UP :: Bit
    , iBTN_DOWN :: Bit
    , iBTN_LEFT :: Bit
    , iBTN_RIGHT :: Bit
    }
    deriving (Show)

data OUTPUT = OUTPUT{
    oVGA_HSYNC :: Bit
    , oVGA_VSYNC :: Bit
    , oVGA_DE :: Bit
    , oVGA_RED :: Word8
    , oVGA_GREEN :: Word8
    , oVGA_BLUE :: Word8
    }
    deriving (Show)


#include "Interface.h"

data Sim

foreign import ccall unsafe "vinit" simInit :: IO (Ptr Sim)
foreign import ccall unsafe "vshutdown" simShutdown :: Ptr Sim -> IO ()
foreign import ccall unsafe "vstep" simStep :: Ptr Sim -> Ptr INPUT -> Ptr OUTPUT -> IO ()

instance Storable Bit where
    alignment = alignment . bitToBool
    sizeOf = sizeOf . bitToBool
    peek = fmap boolToBit . peek . castPtr
    poke ptr = poke (castPtr ptr) . bitToBool

instance Storable INPUT where
    alignment _ = #alignment INPUT
    sizeOf _ = #size INPUT
    {-# INLINE peek #-}
    peek ptr = const INPUT <$> pure ()
        <*> (#peek INPUT, RESET) ptr
        <*> (#peek INPUT, BTN_UP) ptr
        <*> (#peek INPUT, BTN_DOWN) ptr
        <*> (#peek INPUT, BTN_LEFT) ptr
        <*> (#peek INPUT, BTN_RIGHT) ptr
    {-# INLINE poke #-}
    poke ptr INPUT{..} = do
        (#poke INPUT, RESET) ptr iRESET
        (#poke INPUT, BTN_UP) ptr iBTN_UP
        (#poke INPUT, BTN_DOWN) ptr iBTN_DOWN
        (#poke INPUT, BTN_LEFT) ptr iBTN_LEFT
        (#poke INPUT, BTN_RIGHT) ptr iBTN_RIGHT

instance Storable OUTPUT where
    alignment _ = #alignment OUTPUT
    sizeOf _ = #size OUTPUT
    {-# INLINE peek #-}
    peek ptr = const OUTPUT <$> pure ()
        <*> (#peek OUTPUT, VGA_HSYNC) ptr
        <*> (#peek OUTPUT, VGA_VSYNC) ptr
        <*> (#peek OUTPUT, VGA_DE) ptr
        <*> (#peek OUTPUT, VGA_RED) ptr
        <*> (#peek OUTPUT, VGA_GREEN) ptr
        <*> (#peek OUTPUT, VGA_BLUE) ptr
    {-# INLINE poke #-}
    poke ptr OUTPUT{..} = do
        (#poke OUTPUT, VGA_HSYNC) ptr oVGA_HSYNC
        (#poke OUTPUT, VGA_VSYNC) ptr oVGA_VSYNC
        (#poke OUTPUT, VGA_DE) ptr oVGA_DE
        (#poke OUTPUT, VGA_RED) ptr oVGA_RED
        (#poke OUTPUT, VGA_GREEN) ptr oVGA_GREEN
        (#poke OUTPUT, VGA_BLUE) ptr oVGA_BLUE
