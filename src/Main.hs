-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/unclechu-i3-status/master/LICENSE

{-# LANGUAGE PackageImports, UnicodeSyntax, LambdaCase, TupleSections #-}
{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
{-# LANGUAGE BangPatterns, ViewPatterns, MultiWayIf #-}
{-# LANGUAGE DuplicateRecordFields, NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import                Prelude hiding (getLine)
import                Prelude.Unicode

import "data-default" Data.Default (def)
import "base"         Data.Word (Word8, Word32)
import "base"         Data.Fixed (Pico)
import "base"         Data.Maybe (fromMaybe, listToMaybe, catMaybes)
import "base"         Data.Function (fix)
import "aeson"        Data.Aeson (encode, decodeStrict)
import "bytestring"   Data.ByteString.Char8 (getLine, uncons)
import "base"         Data.IORef (newIORef, readIORef, writeIORef)
import "time"         Data.Time.Clock (UTCTime)

import "time"         Data.Time.LocalTime
                        ( TimeZone
                        , TimeOfDay (todSec)
                        , LocalTime (localTimeOfDay)
                        , ZonedTime ( zonedTimeToLocalTime
                                    , zonedTimeZone
                                    )

                        , getZonedTime
                        , zonedTimeToUTC
                        )

import "qm-interpolated-string" Text.InterpolatedString.QM (qm, qms)

import "base" Control.Monad (when, forever, guard, void, join)
import "base" Control.Applicative ((<|>))
import "base" Control.Concurrent (forkIO, threadDelay)
import "base" Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import qualified "async" Control.Concurrent.Async as Async

import "base" System.IO (stderr, hPutStrLn, hFlush)

import "unix" System.Posix.Signals ( installHandler
                                   , Handler (Catch)
                                   , sigHUP
                                   , sigINT
                                   , sigTERM
                                   , sigPIPE
                                   )

import "X11"  Graphics.X11.Types ( xK_Num_Lock
                                 , xK_Caps_Lock
                                 , xK_Shift_L
                                 , xK_Shift_R
                                 )

import "X11"  Graphics.X11.Xlib (openDisplay, closeDisplay)

import "dbus" DBus ( Signal (signalBody, signalSender, signalDestination)
                   , signal
                   , IsVariant (fromVariant, toVariant)
                   , variantType
                   , Type (TypeBoolean, TypeWord8)
                   )

import "dbus" DBus.Client ( connectSession
                          , disconnect
                          , requestName
                          , releaseName
                          , RequestNameReply (NamePrimaryOwner)
                          , addMatch
                          , removeMatch
                          , matchAny
                          , emit
                          , MatchRule ( matchPath
                                      , matchDestination
                                      , matchInterface
                                      , matchMember
                                      )
                          )

-- local imports

import UnclechuI3Status.Battery (setUpBatteryIndicator)
import UnclechuI3Status.ParentProc (dieWithParent)
import UnclechuI3Status.Render (render)
import UnclechuI3Status.Utils
import UnclechuI3Status.WindowTitle (setUpWindowTitle)
import UnclechuI3Status.X (initThreads, fakeKeyEvent)

import UnclechuI3Status.Types
  ( State (..)
  , ProtocolInitialization (..)
  , ClickEvent (..)
  , XmonadrcIfaceParams (..)
  , XlibKeysHackIfaceParams (..)
  , ChangeEvent (..)
  , EventContainer (..)
  , EventContainerWindowProperties (..)
  , EventWorkspace (..)
  , WindowTree (..)
  )



fetchDateAndTime ∷ IO (Pico, UTCTime, TimeZone)
fetchDateAndTime = getZonedTime <&> \zt →

  let utc      = zonedTimeToUTC zt
      timeZone = zonedTimeZone  zt
      seconds  = todSec $ localTimeOfDay $ zonedTimeToLocalTime zt
      secsLeft = 60 - seconds -- left to next minute

   in (secsLeft, utc, timeZone)


data HandleClickEventInterface
   = HandleClickEventInterface
   { toggleAlternativeMode ∷ IO ()
   , getCurrentKbdLayout   ∷ IO (Maybe Layout)
   }

handleClickEvent ∷ HandleClickEventInterface → ClickEvent → IO ()
handleClickEvent iface (\x → name (x ∷ ClickEvent) → Just name') = case name' of

  "numlock"     → fakeKeyEvent $ map (xK_Num_Lock,)  [False, True, False]
  "capslock"    → fakeKeyEvent $ map (xK_Caps_Lock,) [False, True, False]
  "datentime"   → spawnProc "gnome-calendar" []
  "alternative" → toggleAlternativeMode iface

  ['k','b','d','l','a','y','o','u','t','-',a,b] →
    let
      enum = [minBound .. maxBound ∷ Layout]
      next = foldr reducer [] [False, True, False]
        where reducer s acc = (xK_Shift_L, s) : (xK_Shift_R, s) : acc
      switchTo curLayout toLayout = join $ replicate n next
        where
          n = fix (\f x@(l:ls) → if l ≡ curLayout then x else f ls) (cycle enum)
            & fix (\f i (l:ls) → if l ≡ toLayout then i else f (succ i) ls)
                  (0 ∷ Int)
      resolve = do
        layout ← getCurrentKbdLayout iface
        fakeKeyEvent ∘ maybe next (uncurry switchTo) $ (,)
          <$> layout
          <*> foldl (\acc l → if show l ≡ [a,b] then Just l else acc)
                Nothing enum
    in
      resolve

  _ → pure ()

handleClickEvent _ _ = pure ()


type Message = String
type Color = String

-- | Reactive loop that gets state modifier from an @MVar@
--   to update the state and re-render it (if it's @Just@)
--   or terminate the application (it it's @Nothing@).
--
-- This function should be ran in its own thread and it should be the only
-- thread that writes to stdout.
stateModifyHandler
  ∷ (Message → Color → IO ())
  -- ^ Report via Dzen (keyboard layout code or mode like “num” for num lock)
  → IO (Maybe (State → State))
  -- ^ Read next application state modification function
  --   (blocks where there are no updates to handle;
  --   @Nothing@ we are done with the handling, application ends)
  → (State → IO ())
  -- ^ Write new application state (after receiving an update for it)
  → State
  → IO ()
stateModifyHandler reportCallback getNextState writeState initialState = go where
  go = void $ echo (render initialState) >> loop initialState
  loop s = getNextState >>= handle s >>= maybe (pure Nothing) loop
  darn = reportCallback "ERR" "#ff0000"

  -- | Handle one state modification
  --
  -- @Nothing@ means the end of the function, for the both @Maybe@ arguments.
  handle ∷ State → Maybe (State → State) → IO (Maybe State)
  handle _ Nothing = Nothing <$ echo "]"
  handle prevState (Just stateModifier) = Just <$> f where
    f = if newState ≡ prevState then pure prevState else newStateHandler
    newState = stateModifier prevState

    newStateHandler = newState <$ do
      writeState newState
      echo $ "," ⋄ render newState

      if
        | kbdLayout newState ≢ kbdLayout prevState →
            case kbdLayout newState of
              Just (Right layout) →
                reportCallback (show layout) (colorOfLayout layout)
              _ → darn

        | alternative newState ≢ alternative prevState
        ∧ ( fmap snd (alternative newState)  ≡ Just True
          ∨ fmap snd (alternative prevState) ≡ Just True
          ) →
            either (const darn) (uncurry reportCallback) $ (,)
              <$> (showAlternativeState . alternative) newState
              <*> (colorOfAlternativeState . alternative) newState

        | capsLock newState ≢ capsLock prevState →
            reportCallback
              (showCapsLock . capsLock $ newState)
              (colorOfCapsLock . capsLock $ newState)

        | numLock newState ≢ numLock prevState →
            reportCallback
              (showNumLock . numLock $ newState)
              (colorOfNumLock . numLock $ newState)

        | otherwise → pure ()


main ∷ IO ()
main = do
  initThreads

  -- Connecting to DBus
  client  ← connectSession

  -- Getting bus name for our service that depends on Display name
  dpyView ← do dpy    ← openDisplay ""
               let !x = getDisplayName dpy
               x <$ closeDisplay dpy

  -- Grab the bus name for our service
  requestName client (busName (def ∷ XmonadrcIfaceParams) dpyView) []
    >>= \reply →
          when (reply ≢ NamePrimaryOwner) $
            fail [qms|
              Requesting name
              '{busName (def ∷ XmonadrcIfaceParams) dpyView}'
              error: {reply}
            |]

  mVar ← newEmptyMVar

  let put = putMVar mVar

      basicMatchRule = matchAny
        { matchPath        = Just $ objPath (def ∷ XmonadrcIfaceParams)
        , matchInterface   = Just $ interfaceName (def ∷ XmonadrcIfaceParams)
        , matchDestination = Just $ busName (def ∷ XmonadrcIfaceParams) dpyView
        }

  -- If `xlib-keys-hack` started before ask it to reflush indicators
  emit client ( signal (flushObjPath  (def ∷ XmonadrcIfaceParams) dpyView)
                       (interfaceName (def ∷ XmonadrcIfaceParams))
                       "request_flush_all"
              ) { signalSender =
                    Just $ busName (def ∷ XmonadrcIfaceParams) dpyView
                , signalDestination = Nothing
                , signalBody = []
                }

  -- Bind IPC events handlers
  sigHandlers ←
    let listen (member, stateModifier) =
          addMatch client (matchRule member) $ handle stateModifier

        matchRule member = basicMatchRule { matchMember = Just member }
        flag = fromMaybe False ∘ fromVariant

        handle stateModifier (signalBody → body) = case variantType <$> body of
          [TypeBoolean]            → put $ Just $ stateModifier body
          [TypeWord8]              → put $ Just $ stateModifier body
          [TypeWord8, TypeBoolean] → put $ Just $ stateModifier body
          _ → pure () -- Incorrect arguments, just ignoring it

        oneArg f [x] = f x
        oneArg _ _   = Prelude.id

        twoArgs f [a, b] = f (a, b)
        twoArgs _ _      = Prelude.id

        parseLayout
          = fromVariant
          • fmap (\n → Left (Just n) `maybe` Right $ numToLayout n)
          • (<|> Just (Left Nothing))

     in mapM listen

             -- Pairs of IPC method and state modifir
             [ ("numlock",   oneArg $ \x s → s { numLock   = flag x })
             , ("capslock",  oneArg $ \x s → s { capsLock  = flag x })
             , ("xkblayout", oneArg $ \x s → s { kbdLayout = parseLayout x })

             -- Support @"alternative"@ for backward compatibility
             , ( "alternative"
               , oneArg $ \(fromVariant → x ∷ Maybe Bool) s → s
                   { alternative = (x >>= guard) $> (1 ∷ Word8, False) }
               )

             , ( "alternative_level"
               , twoArgs $ \(level, isPermanent) s → s
                   { alternative = do
                       level'       ← fromVariant level
                       isPermanent' ← fromVariant isPermanent

                       -- @0@/@minBound@ level means
                       -- alternative mode is turned off (@Nothing@).
                       guard $ level' > minBound

                       Just (level', isPermanent')
                   }
               )
             ]

  -- Fetcing date and time thread
  _ ← forkIO $ forever $ do
    (secondsLeftToNextMinute, utc, timeZone) ← fetchDateAndTime
    put $ Just $ \s → s { lastTime = Just (utc, timeZone) }
    threadDelay $ ceiling $ secondsLeftToNextMinute × 1000 × 1000

  !batteryData ← setUpBatteryIndicator $ \case
    (Nothing, Nothing) → pure ()

    (Just chargeLeft, Just chargeState) →
      put $ Just $ \s → s { battery = Just (chargeLeft, chargeState) }

    (Just chargeLeft, Nothing) →
      put $ Just $ \s → s
        { battery = battery s >>= Just ∘ (chargeLeft,) ∘ snd }

    (Nothing, Just chargeState) →
      put $ Just $ \s → s
        { battery = battery s >>= Just ∘ (,chargeState) ∘ fst }

  !windowTitleData ← setUpWindowTitle $ \case
    Left msg → do
      hPutStrLn stderr [qm| Error while parsing window title event: {msg} |]
      hFlush stderr
      put $ Just $ \s → s { windowTitle = Nothing }
    Right ev → case ev of
      WindowFocusEvent { container } →
        when (focused (container ∷ EventContainer)) $
          put $ Just $ \s → s
            { windowTitle = Just $
                title $ windowProperties (container ∷ EventContainer)
            }
      WindowTitleEvent { container } →
        when (focused (container ∷ EventContainer)) $
          put $ Just $ \s → s
            { windowTitle = Just $
                title $ windowProperties (container ∷ EventContainer)
            }
      WindowCloseEvent { container } →
        when (focused (container ∷ EventContainer)) $
          put $ Just $ \s → s { windowTitle = Nothing }

      WorkspaceFocusEvent { current } → let
        f x@WindowTree { focused = True } = Just x
        f WindowTree { nodes } = firstFocused nodes

        firstFocused = listToMaybe ∘ catMaybes ∘ map f
        top = firstFocused $ nodes (current ∷ EventWorkspace)
        t = top >>= \x → title <$> windowProperties (x ∷ WindowTree)

        in put $ Just $ \s → s { windowTitle = t }

      OtherEvent _ → pure ()

  let defState ∷ State
      defState
        = def
        { battery     = fst <$> batteryData
        , windowTitle = fst windowTitleData
        }

  stateRef ← newIORef defState

  let
    toggleAlternativeMode' = do
      (newAlternativeState :: Word32) ←
        readIORef stateRef <&> alternative <&> \case
          Nothing     → 1
          Just (1, _) → 2
          _           → 0

      emit client ( signal (objPath (def ∷ XlibKeysHackIfaceParams))
                           (interfaceName (def ∷ XlibKeysHackIfaceParams))
                           "switch_alternative_mode"
                  ) { signalSender =
                        Just $ busName (def ∷ XmonadrcIfaceParams) dpyView
                    , signalDestination =
                        Just $ busName (def ∷ XlibKeysHackIfaceParams) dpyView
                    , signalBody = [toVariant newAlternativeState]
                    }

    handleEv
      = handleClickEvent HandleClickEventInterface
      { toggleAlternativeMode = toggleAlternativeMode'
      , getCurrentKbdLayout
          = readIORef stateRef <&> kbdLayout
          • fmap (either (const Nothing) Just) • join
      }

  -- Reading click events from i3-bar
  _ ← forkIO $ do
    "[" ← getLine -- Opening of lazy list
    do -- First one (without comma)
      Just ev ← decodeStrict <$> getLine
      handleEv ev
    forever $ do
      Just ev ← getLine <&> \(uncons → Just (',', x)) → decodeStrict x
      handleEv ev

  -- Handle POSIX signals to terminate application
  let terminate = do maybe (pure ()) snd batteryData -- unsubscribe
                     snd windowTitleData -- unsubscribe
                     mapM_ (removeMatch client) sigHandlers
                     _ ← releaseName client
                       $ busName (def ∷ XmonadrcIfaceParams) dpyView
                     disconnect client
                     put Nothing

      catch sig = installHandler sig (Catch terminate) Nothing

   in mapM_ catch [sigHUP, sigINT, sigTERM, sigPIPE]

  dieWithParent -- make this app die if parent die

  echo $ encode (def ∷ ProtocolInitialization) { clickEvents = True }
  echo "[" -- Opening of lazy list

  dzen'
    ← newIORef Nothing <&>
    \ ref text color → void . Async.async $ dzen ref text color

  stateModifyHandler dzen' (takeMVar mVar) (writeIORef stateRef) defState
