-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/unclechu-i3-status/master/LICENSE

{-# LANGUAGE UnicodeSyntax, PackageImports, BangPatterns, ViewPatterns #-}
{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Battery
     ( setUpBatteryIndicator
     ) where

import "base" Data.Word (Word8)
import "base" Data.Either (isRight)
import "base" Data.Foldable (find)
import "base" Data.String (fromString)
import "qm-interpolated-string" Text.InterpolatedString.QM (qms)
import qualified "containers" Data.Map.Strict as Map
import qualified "attoparsec" Data.Attoparsec.ByteString.Char8 as Parsec

import "dbus" DBus ( Signal (signalBody)
                   , Variant
                   , IsVariant (fromVariant, toVariant)
                   , MethodCall (methodCallDestination, methodCallBody)
                   , methodCall
                   , MethodReturn (methodReturnBody)
                   , ObjectPath
                   , formatObjectPath
                   )

import "dbus" DBus.Client ( Client
                          , connectSystem
                          , addMatch
                          , removeMatch
                          , matchAny
                          , call_
                          , SignalHandler
                          , MatchRule ( matchPath
                                      , matchInterface
                                      , matchMember
                                      )
                          )

-- local imports

import Utils
import Types (UPowerBatteryState (..))


type UPowerPropName = String

setUpBatteryIndicator
  ∷ ((Maybe Double, Maybe UPowerBatteryState) → IO ()) -- Update handler
  → IO (Maybe ((Double, UPowerBatteryState), IO ())) -- Initial and unsubscriber

setUpBatteryIndicator updateHandler = do
  client ← connectSystem

  !batteryObjPath ←
    call_ client ( methodCall "/org/freedesktop/UPower"
                              "org.freedesktop.UPower"
                              "EnumerateDevices"
                 ) { methodCallDestination = Just "org.freedesktop.UPower" }

      <&!> \reply → let
             objPaths = [ y | Just x ← fromVariant <$> methodReturnBody reply
                            , y ← (x ∷ [ObjectPath]) ]

             -- …/battery_BAT0
             parser = ()
               <$ Parsec.manyTill' Parsec.anyChar "/battery_BAT"
               <* (Parsec.decimal ∷ Parsec.Parser Word8)
               <* Parsec.endOfInput

             batteryObjPath = find ( isRight
                                   ∘ Parsec.parseOnly parser
                                   ∘ fromString
                                   ∘ formatObjectPath
                                   ) objPaths ∷ Maybe ObjectPath

             in batteryObjPath

  case batteryObjPath of
       Nothing → pure Nothing
       Just !x → do
         unsubscriber ← removeMatch client <$> catchUpdate client x
         chargeLeft   ← getPropCall client x "Percentage"
         chargeState  ← getPropCall client x "State"
         pure $ Just ((chargeLeft, chargeState), unsubscriber)

  where
    -- Method call to gets a property of a battery device
    getPropCall ∷ IsVariant α ⇒ Client → ObjectPath → UPowerPropName → IO α
    getPropCall client batteryObjPath propName =
      call_ client propCall <&!> \reply →
        case methodReturnBody reply of

             [x] → case fromVariant x >>= fromVariant of
                        Nothing → error [qms| Unexpected UPower reply: {x} |]
                        Just y  → y

             x   → error [qms| Unexpected UPower reply: {x} |]

      where
        propCall =
          (methodCall batteryObjPath "org.freedesktop.DBus.Properties" "Get")
            { methodCallDestination = Just "org.freedesktop.UPower"
            , methodCallBody =
                toVariant <$> ["org.freedesktop.UPower.Device", propName]
            }

    catchUpdate ∷ Client → ObjectPath → IO SignalHandler
    catchUpdate client betteryObj = addMatch client rule $ handler ∘ signalBody
      where
        propsToWatch = ["Percentage", "State"] ∷ [String]

        handler [ fromVariant → Just ("org.freedesktop.UPower.Device" ∷ String)

                , fromVariant →
                    Just props@(Map.keys → any (∈ propsToWatch) → True)
                      ∷ Maybe (Map.Map String Variant)

                , fromVariant → Just (_ ∷ [Variant])
                ]
                = updateHandler
                    ( fromVariant =<< Map.lookup "Percentage" props
                    , fromVariant =<< Map.lookup "State"      props
                    )

        handler x =
          error [qms| Unexpected UPower property changed signal body: {x} |]

        rule
          = matchAny
          { matchPath      = Just betteryObj
          , matchInterface = Just "org.freedesktop.DBus.Properties"
          , matchMember    = Just "PropertiesChanged"
          }
