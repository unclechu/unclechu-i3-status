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
import UnclechuI3Status.Layout (Layout)


data State
  = State
  { numLock ∷ Bool
  , capsLock ∷ Bool

  , alternative ∷ Maybe (Word8, Bool)
  -- ^ Alternative mode state
  --   (@Bool@ indicates whether alternative mode is turned on permanently)

  , kbdLayout ∷ Maybe (Either Word8 Layout)
  -- ^ Keyboard layout (@Left@ contains unknown layout number
  --   if parsing of "Layout" has failed)

  , lastTime ∷ Maybe (UTCTime, TimeZone)
  -- ^ Current date-time
  --   (the reason why don't just store "ZonedTime" here
  --   is that it doesn't have "Eq" instance,
  --   see https://github.com/haskell/time/issues/50 for details)

  , battery ∷ Maybe (Double, UPowerBatteryState)
  -- ^ Battery charge state

  , windowTitle ∷ Maybe String
  -- ^ The title of currenlty focused window
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
