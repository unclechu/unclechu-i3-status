-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/unclechu-i3-status/master/LICENSE

{-# LANGUAGE PackageImports #-}
{-# LANGUAGE UnicodeSyntax #-}

-- | Module responsible of handling application state
module UnclechuI3Status.Handler.AppState.Types
     ( State (..)
     ) where

import "base" Data.Word (Word8)
import "data-default" Data.Default (Default (def))
import "time" Data.Time.Clock (UTCTime)
import "time" Data.Time.LocalTime (TimeZone)

-- Local imports

import UnclechuI3Status.EventSubscriber.Battery (UPowerBatteryState)
import UnclechuI3Status.Utils (Layout)


data State
  = State
  { numLock ∷ Bool
  , capsLock ∷ Bool

  , alternative ∷ Maybe (Word8, Bool)
  -- ^ @Bool@ indicates whether alternative mode is turned on permanently

  , kbdLayout ∷ Maybe (Either (Maybe Word8) Layout)
  -- ^ First @Maybe@ indicates whether value have been received, second
  --   nested @Maybe@ inside @Left@ indicates whether it is failed to parse
  --   even just a number.
  --
  -- @Left@ contains unknown number of layout.

  , lastTime ∷ Maybe (UTCTime, TimeZone)
  -- ^ The reason why don't just store ZonedTime here
  --   is that it doesn't have Eq instance.
  --   See https://github.com/haskell/time/issues/50

  , battery ∷ Maybe (Double, UPowerBatteryState)
  , windowTitle ∷ Maybe String
  }
  deriving (Show, Eq)

instance Default State where
  def
    = State
    { numLock = False
    , capsLock = False
    , alternative = Nothing
    , kbdLayout = Nothing
    , lastTime = Nothing
    , battery = Nothing
    , windowTitle = Nothing
    }
