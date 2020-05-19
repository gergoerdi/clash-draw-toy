{-# LANGUAGE NumericUnderscores, RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
module DrawToy where

import Clash.Prelude
import Clash.Annotations.TH
import RetroClash.Utils
import RetroClash.Barbies
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
    board btns = delayVGA vgaSync rgb
      where
        VGADriver{..} = vgaDriver vga640x480at60
        frameEnd = isFalling False (isJust <$> vgaY)

        x = center . scale (SNat @5) $ vgaX
        y = center . scale (SNat @5) $ vgaY
        rgb = drawToy frameEnd input x y

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
    :: (KnownNat r, KnownNat g, KnownNat b)
    => (HiddenClockResetEnable dom)
    => Signals dom VGASync
    -> Signal dom (Unsigned r, Unsigned g, Unsigned b)
    -> VGAOut dom r g b
delayVGA VGASync{..} rgb = vgaOut sync' rgb
  where
    sync' = VGASync
        { vgaHSync = register undefined vgaHSync
        , vgaVSync = register undefined vgaVSync
        , vgaDE = register undefined vgaDE
        }

drawToy
    :: (HiddenClockResetEnable dom)
    => Signal dom Bool
    -> Signal dom (Maybe Move)
    -> Signal dom (Maybe (Index 128))
    -> Signal dom (Maybe (Index 64))
    -> Signal dom (Unsigned 8, Unsigned 8, Unsigned 8)
drawToy frameEnd input x y = rgb
  where
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

    fbRead = blockRam1 ClearOnReset (SNat @(2^(6 + 7))) False fbAddr fbWrite
    fbWrite = do
        ~(x, y) <- cursor
        pure $ Just (bitCoerce @_ @(Unsigned _) (y, x), True)

    fbAddr = bitCoerce <$> bundle (ry, rx)

    rgb = mux (not <$> visible) (pure (0, 0, 0)) $
          mux (cursor .==. bundle (rx, ry)) (cursorColor <$> cursorState) $
          pixel <$> fbRead

    pixel True  = (240, 200, 255)
    pixel False = (32,  32,  32)

    cursorColor True  = (255, 255, 255)
    cursorColor False = (128, 128, 128)

makeTopEntity 'topEntity
