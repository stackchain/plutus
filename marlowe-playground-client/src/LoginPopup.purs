module LoginPopup where

import Prelude
import Auth (AuthRole)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..))
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Aff (Aff, finally, makeAff, nonCanceler)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Foreign.Generic.Class (encode, decode)
import Web.Event.Event (EventType(..), Event)
import Web.Event.EventTarget (EventListener, addEventListener, eventListener, removeEventListener)
import Web.HTML as Web
import Web.HTML.Location (replace)
import Web.HTML.Window (outerHeight, outerWidth)
import Web.HTML.Window as Window
import Web.HTML.WindowExtra (close, postMessage)
import Web.Socket.Event.MessageEvent as MessageEvent

-- This function is intended to be called between a parent window. It creates a popup to the
-- the Github auth page and waits for the popup to send a message with the current AuthRole
openLoginPopup :: Aff AuthRole
openLoginPopup = do
  window <- liftEffect Web.window
  let
    popupHeight = 620

    popupWidth = 600

    githubLoginPage = "/api/oauth/github"

    features :: Aff String
    features =
      liftEffect do
        top <-
          outerHeight window
            <#> \windowHeight -> windowHeight / 2 - popupHeight / 2
        left <-
          outerWidth window
            <#> \windowWidth -> windowWidth / 2 - popupWidth / 2
        pure $ "width="
          <> show popupWidth
          <> ",height="
          <> show popupHeight
          <> ",top="
          <> show top
          <> ",left="
          <> show left
          <> ",menubar=no,status=no,location=no"

    decodeMessageEvent :: Event -> Maybe AuthRole
    decodeMessageEvent event = do
      data' <- MessageEvent.data_ <$> MessageEvent.fromEvent event
      hush <<< runExcept <<< decode $ data'

    waitForEvent :: Ref (Maybe EventListener) -> Aff AuthRole
    waitForEvent listenerRef = makeAff resolver
      where
      resolver cb = do
        -- This callback listens for all "message" events, but only succeeds when
        -- we can decode the event.data as an AuthRole
        listener <-
          eventListener \event -> case decodeMessageEvent event of
            Nothing -> pure unit
            Just role -> cb $ Right role
        Ref.write (Just listener) listenerRef
        addEventListener (EventType "message") listener false $ Window.toEventTarget window
        -- We can return a nonCanceler because the waitForEvent is called with a finally
        pure nonCanceler

    removeWaitForEventListener :: Ref (Maybe EventListener) -> Aff Unit
    removeWaitForEventListener listenerRef =
      liftEffect do
        mbListener <- Ref.read listenerRef
        for_ mbListener \listener ->
          removeEventListener (EventType "message") listener false (Window.toEventTarget window)
  featureString <- features
  _ <- liftEffect $ Window.open githubLoginPage "_blank" featureString window
  listenerRef <- liftEffect $ Ref.new Nothing
  -- Make sure that the event listener is removed no matter what
  finally
    (removeWaitForEventListener listenerRef)
    (waitForEvent listenerRef)

-- This function is intended to be called from the popup window created by openLoginPopup.
informParentAndClose :: AuthRole -> Effect Unit
informParentAndClose authRole = do
  window <- Web.window
  Window.opener window
    >>= case _ of
        -- If the function is called from a poput window (expected behaviour)
        -- then we comunicate the login result with our parent window and close
        -- the popup
        Just parent -> do
          postMessage (encode authRole) parent
          close window
        -- If someone access the github callback url directly, we redirect them to
        -- the home page
        Nothing -> Window.location window >>= replace "/"