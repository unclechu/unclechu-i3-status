-- Author: Viacheslav Lotsmanov
-- License: GPLv3 https://raw.githubusercontent.com/unclechu/unclechu-i3-status/master/LICENSE

{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE UnicodeSyntax #-}

-- | Module responsible of handling application state
module UnclechuI3Status.Handler.AppState
     ( appStateHandler
     , module UnclechuI3Status.Handler.AppState.Types
     ) where

import "base" Control.Monad (void)

-- Local imports

import UnclechuI3Status.Handler.AppState.Types
import UnclechuI3Status.Layout (colorOfLayout)
import UnclechuI3Status.Render (render)
import UnclechuI3Status.Utils


type Message = String
type Color = String

-- | Reactive loop that gets state modifier from an @MVar@
--   to update the state and re-render it (if it's @Just@)
--   or terminate the application (it it's @Nothing@).
--
-- This function should be ran in its own thread and it should be the only
-- thread that writes to stdout.
--
-- This function is a store itself for an application "State".
-- It takes a function that reads next "State" modification but the "State"
-- itself is preserved within this function. Well, it also takes a function to
-- write the new "State" to just to let some other threads to read it for their
-- own purposes.
appStateHandler
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
appStateHandler reportCallback getNextStateUpdate writeState initialState = go where
  go = void $ echo "[" >> echo (render initialState) >> loop initialState
  loop s = getNextStateUpdate >>= handle s >>= maybe (pure Nothing) loop
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

      if | kbdLayout newState ≢ kbdLayout prevState →
             case kbdLayout newState of
               Just (Right layout) →
                 reportCallback (show layout) (colorOfLayout layout)
               _ → darn

         | alternative newState ≢ alternative prevState
         ∧ ( fmap snd (alternative newState) ≡ Just True
           ∨ fmap snd (alternative prevState) ≡ Just True
           ) →
             either (const darn) (uncurry reportCallback) $ (,)
               <$> (showAlternativeState ∘ alternative) newState
               <*> (colorOfAlternativeState ∘ alternative) newState

         | capsLock newState ≢ capsLock prevState →
             reportCallback
               (showCapsLock ∘ capsLock $ newState)
               (colorOfCapsLock ∘ capsLock $ newState)

         | numLock newState ≢ numLock prevState →
             reportCallback
               (showNumLock ∘ numLock $ newState)
               (colorOfNumLock ∘ numLock $ newState)

         | otherwise → pure ()
