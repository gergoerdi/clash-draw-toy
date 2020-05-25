{-# LANGUAGE NumericUnderscores, RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
module DrawToy where

import Clash.Prelude
import Clash.Annotations.TH
import RetroClash.Utils
import RetroClash.VGA
import RetroClash.Video
import RetroClash.Clock
import Data.Maybe

-- | 25 MHz clock, needed for the VGA mode we use.
createDomain vSystem{vName="Dom25", vPeriod = hzToPeriod 25_175_000}

topEntity
    :: "CLK_25MHZ" ::: Clock Dom25
    -> "RESET" ::: Reset Dom25
    -> "BTN" ::: ( "UP"    ::: Signal Dom25 (Active High)
                 , "DOWN"  ::: Signal Dom25 (Active High)
                 , "LEFT"  ::: Signal Dom25 (Active High)
                 , "RIGHT" ::: Signal Dom25 (Active High))
    -> "VGA" ::: VGAOut Dom25 8 8 8
topEntity = withEnableGen board
  where
    board btns = vga
      where
        (frameEnd, vga) = drawToy input

        input = fromButtons <$> bundle (up', dn', lt', rt')
          where
            (up, dn, lt, rt) = btns
            up' = fromActive <$> up
            dn' = fromActive <$> dn
            lt' = fromActive <$> lt
            rt' = fromActive <$> rt

data Move
    = MoveUp
    | MoveDown
    | MoveLeft
    | MoveRight

fromButtons :: (Bool, Bool, Bool, Bool) -> Maybe Move
fromButtons (True, _, _, _) = Just MoveUp
fromButtons (_, True, _, _) = Just MoveDown
fromButtons (_, _, True, _) = Just MoveLeft
fromButtons (_, _, _, True) = Just MoveRight
fromButtons _               = Nothing

delayVGA
    :: forall dom d r g b. (HiddenClockResetEnable dom, KnownNat d, KnownNat r, KnownNat g, KnownNat b)
    => VGASync dom
    -> DSignal dom d (Unsigned r, Unsigned g, Unsigned b)
    -> VGAOut dom r g b
delayVGA VGASync{..} rgb = VGAOut{..}
  where
    vgaSync = VGASync
        { vgaHSync = matchDelay rgb vgaHSync
        , vgaVSync = matchDelay rgb vgaVSync
        , vgaDE = matchDelay rgb vgaDE
        }

    matchDelay :: (NFDataX a) => DSignal dom d any -> Signal dom a -> Signal dom a
    matchDelay d = toSignal . (d *>) . toDelayedU

    (vgaR, vgaG, vgaB) = unbundle $ toSignal rgb

drawToy
    :: (HiddenClockResetEnable dom, DomainPeriod dom ~ HzToPeriod 25_175_000)
    => Signal dom (Maybe Move)
    -> (Signal dom Bool, VGAOut dom 8 8 8)
drawToy input = (frameEnd, delayVGA vgaSync rgb)
  where
    VGADriver{..} = vgaDriver vga640x480at60
    frameEnd = isFalling False (isJust <$> vgaY)

    x = center @128 . scale (SNat @5) $ vgaX
    y = center @64 . scale (SNat @5) $ vgaY

    rx = fromMaybe 0 <$> x
    ry = fromMaybe 0 <$> y
    visible = isJust <$> x .&&. isJust <$> y

    cursor = regEn ((0 :: Index 128), (0 :: Index 64)) frameEnd $ do
        ~(x, y) <- cursor
        input <- input
        pure $ case input of
            Just MoveUp    -> (x,         prevIdx y)
            Just MoveDown  -> (x,         nextIdx y)
            Just MoveLeft  -> (prevIdx x, y)
            Just MoveRight -> (nextIdx x, y)
            Nothing        -> (x,         y)

    frameCounter = regEn (0 :: Index 10) frameEnd $ nextIdx <$> frameCounter
    cursorState = regEn True (frameEnd .&&. frameCounter .== 0) $ not <$> cursorState

    fbRead = delayedBlockRam1 ClearOnReset (SNat @(128 * 64)) False fbAddr fbWrite

    fbWrite = fromSignal $ do
        ~(x, y) <- cursor
        pure $ Just (bitCoerce @_ @(Unsigned _) (y, x), True)

    fbAddr = fromSignal $ bitCoerce <$> bundle (ry, rx)

    rgb = mux (delayedI False . fromSignal $ not <$> visible) (pure (0, 0, 0)) $
          mux (toDelayedU $ cursor .==. bundle (rx, ry)) (toDelayedU $ cursorColor <$> cursorState) $
          pixel <$> fbRead

    pixel True  = (240, 200, 255)
    pixel False = (32,  32,  32)

    cursorColor True  = (255, 255, 255)
    cursorColor False = (128, 128, 128)

delayedU
  :: (KnownNat d, NFDataX a, HiddenClockResetEnable dom)
  => DSignal dom n a
  -> DSignal dom (n + d) a
delayedU = delayedI undefined

toDelayedU
  :: (KnownNat d, NFDataX a, HiddenClockResetEnable dom)
  => Signal dom a
  -> DSignal dom d a
toDelayedU = delayedU . fromSignal

delayedBlockRam1
    :: (1 <= m, Enum addr, NFDataX a, HiddenClockResetEnable dom)
    => ResetStrategy r
    -> SNat m
    -> a
    -> DSignal dom d addr
    -> DSignal dom d (Maybe (addr, a))
    -> DSignal dom (d + 1) a
delayedBlockRam1 resetStrat size content addr wr = unsafeFromSignal $
    blockRam1 resetStrat size content (toSignal addr) (toSignal wr)

makeTopEntity 'topEntity
