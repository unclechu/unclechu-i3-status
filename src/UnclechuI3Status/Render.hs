{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}

-- | Serializing the state of the application into a string
module UnclechuI3Status.Render
     ( render
     ) where

import "base-unicode-symbols" Prelude.Unicode

import "aeson" Data.Aeson (encode)
import "base" Data.Tuple (swap)
import "base" Data.Word (Word8)
import "bytestring" Data.ByteString.Lazy.Char8 (ByteString)
import "data-default" Data.Default (def)
import "time" Data.Time.LocalTime (utcToZonedTime)

-- Local imports

import UnclechuI3Status.Handler.AppState.Types (State (..))
import UnclechuI3Status.Types (Unit (..), UPowerBatteryState (..))
import UnclechuI3Status.Utils


-- | Render given state to a string (encoded JSON value)
--
-- This item should be a part of an open streaming list printed to stdout.
-- Adding commas and open of the list is out of the responsibilities of this
-- function. It should be handled somewhere else.
render ∷ State → ByteString
render s
  = (encode ∷ [Unit] → ByteString)
  $ maybe mempty (\x → [windowTitleView x, _separate]) (windowTitle s)
  ⋄
  [ numLockView s
  , capsLockView s
  , alternativeView s
  , _separate
  ]
  ⋄ kbdLayoutView s
  ⋄ [ _separate, dateAndTimeView s ]
  ⋄ maybe mempty (\x → [_separate, batteryView x]) (battery s)


-- * Units

numLockView ∷ State → Unit
numLockView s
  = def
  { fullText = showNumLock isOn
  , color = Just $ colorOfNumLock isOn
  , name = Just "numlock"
  }
  where
    isOn = numLock s


capsLockView ∷ State → Unit
capsLockView s
  = def
  { fullText = showCapsLock isOn
  , color = Just $ colorOfCapsLock isOn
  , name = Just "capslock"
  }
  where
    isOn = capsLock s


alternativeView ∷ State → Unit
alternativeView s
  = def
  { fullText =
      either (\n → "%UNKNOWN:" ⋄ show n ⋄ "%") Prelude.id $
        showAlternativeState alternativeState

  , color =
      either (const Nothing) Just $
        colorOfAlternativeState alternativeState

  , name = Just "alternative"
  }
  where
    alternativeState = alternative s


-- Layout names are just hardcoded, they may be not in this exact order.
kbdLayoutView ∷ State → [Unit]
kbdLayoutView s = go where
  f nameSuffix fullText (Just → color) =
    def { name = Just ("kbdlayout-" ⋄ nameSuffix), fullText, color }

  go = case kbdLayout s of
    Nothing → pure $ f "UNDEFINED" "%UNDEFINED%" "#eeeeee"
    Just (Left Nothing) → pure $ f "ERROR" "%ERROR%" "#ff0000"
    Just (Left (Just n)) →
      pure $ f "UNKNOWN" ("%UNKNOWN:" ⋄ show n ⋄ "%") "#eeeeee"
    Just (Right layout) →
      [minBound .. maxBound ∷ Layout] <&> \x →
        f (show x) (show x) $
          if x ≡ layout then colorOfLayout layout else "#666666"


dateAndTimeView ∷ State → Unit
dateAndTimeView s = go where
  go = maybe def { fullText = "…" } (set ∘ render') $ lastTime s
  render' = renderDate ∘ uncurry utcToZonedTime ∘ swap
  set x  = def { fullText = x, name = Just "datentime" }


batteryView ∷ (Double, UPowerBatteryState) → Unit
batteryView (chargeLeft, batteryState) = go where
  go = def
    { -- Rounding because floating point is always zero
      fullText = icon batteryState ⋄ show (round chargeLeft ∷ Word8) ⋄ "%"

    , name = Just "battery"

    , color =
        Just $ case batteryState of
          Charging → connectedToAdapterColor
          FullyCharged → connectedToAdapterColor
          _ | chargeLeft ≥ 80 → "#00ff00"
            | chargeLeft < 20 → "#ff0000"
            | otherwise → "#ffff00"

    }

  connectedToAdapterColor = "#00ffff"
  dischargingIcon = "🔋"
  chargingIcon = "⚡"

  icon = \case
    Charging → chargingIcon
    FullyCharged → chargingIcon
    _ → dischargingIcon


windowTitleView ∷ String → Unit
windowTitleView x = def
  { fullText = x
  , name = Just "window-title"
  }


_separate ∷ Unit
_separate = def { fullText = "/", color = Just "#666666" }
-- separateAfter x = x { separator = Just True, separatorBlockWidth = Just 20 }