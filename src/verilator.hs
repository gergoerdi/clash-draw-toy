{-# LANGUAGE RecordWildCards, OverloadedStrings, NumericUnderscores #-}
module Main where

import Prelude
import Clash.Prelude (boolToBit)

import RetroClash.VGA
import RetroClash.Sim.SDL
import RetroClash.Sim.VGA
import RetroClash.Sim.VGASDL

import SimInterface
import VerilatorFFI
import Foreign.Storable
import Foreign.Marshal.Alloc

import SDL hiding (get)
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Array.IO
import Control.Lens

import Data.Int
import Text.Printf
import Control.Monad.Extra
import Data.Text

{-# INLINE withRunner #-}
withRunner :: ((INPUT -> IO OUTPUT) -> IO a) -> IO a
withRunner act = alloca $ \inp -> alloca $ \outp -> do
    sim <- simInit
    let step input = do
            poke inp input
            simStep sim inp outp
            peek outp
    x <- act step
    simShutdown sim
    return x

main :: IO ()
main = withRunner $ \runCycle -> do
    buf <- newBufferArray
    t0 <- ticks

    flip evalStateT (initSink, (1, t0)) $ withMainWindow videoParams $ \events keyDown -> do
        guard $ not $ keyDown ScancodeEscape

        let input = INPUT
                { reset = False
                , btnUp = keyDown ScancodeUp
                , btnDown = keyDown ScancodeDown
                , btnLeft = keyDown ScancodeLeft
                , btnRight = keyDown ScancodeRight
                }

        whileM $ do
            vgaOut <- do
                OUTPUT{..} <- liftIO $ runCycle input
                return (vgaHSYNC, vgaVSYNC, (vgaRED, vgaGREEN, vgaBLUE))
            fmap not $ zoom _1 $ lift $ vgaSinkBuf vga640x480at60 buf vgaOut

        zoom _2 $ do
            (i, t0) <- get
            if i == 60 then do
                t <- ticks
                let dt = t - t0
                    fps = 1000 / (fromIntegral dt / 60) :: Double
                liftIO $ printf "60 frames in %d ms, %.1f fps\n" dt fps
                put (1, t)
              else put (i + 1, t0)

        return $ rasterizeBuffer buf
  where
    videoParams = MkVideoParams
        { windowTitle = "Drawing Toy"
        , screenScale = 2
        , screenRefreshRate = 60
        }
