{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module DBus.Proxy
  ( proxyAll
  ) where

import Control.Exception (SomeException, catch)
import Control.Concurrent (threadDelay)
import Control.Monad (forM_, void, when)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (listToMaybe)
import DBus
import DBus.Client
import qualified DBus.Introspection as I
import System.Log.Logger (logM, Priority(..))

-- Proxy every interface found by Introspect on (busName, pathToProxy), and
-- export them at registrationPath on this client connection.
--
-- This is useful when an item is exported via the `dbus` package, but the menu
-- is hosted by `gi-dbusmenu` on a separate bus name. Since the SNI `Menu`
-- property is only an object path (no bus name), most hosts expect to call the
-- DBusMenu interface on the item's bus name. This proxy bridges that gap.
proxyAll :: Client -> BusName -> ObjectPath -> ObjectPath -> IO ()
proxyAll client busName pathToProxy registrationPath = do
  -- gi-dbusmenu object registration can race startup; retry for a short time.
  let maxAttempts = 50 :: Int
      delayUs = 100000 -- 100ms
      go n = do
        eObj <- introspectObject client busName pathToProxy
        case eObj of
          Left err
            | n <= 0 -> logM "DBus.Proxy" WARNING $ "Proxy setup failed: " <> err
            | otherwise -> threadDelay delayUs >> go (n - 1)
          Right obj -> do
            let interfaces = I.objectInterfaces obj
            when (null interfaces) $
              logM "DBus.Proxy" WARNING "No interfaces found when attempting to proxy"
            forM_ interfaces $ \iface ->
              buildAndRegisterInterface client busName pathToProxy registrationPath iface
  go maxAttempts

introspectObject :: Client -> BusName -> ObjectPath -> IO (Either String I.Object)
introspectObject client busName pathToProxy = do
  let callMsg =
        (methodCall pathToProxy
                    (interfaceName_ "org.freedesktop.DBus.Introspectable")
                    (memberName_ "Introspect"))
          { methodCallDestination = Just busName
          }
  reply <- call client callMsg
  pure $ do
    ret <- firstMethodReturn reply
    xmlText <- maybeToEither "Introspect returned no body" $
      listToMaybe (methodReturnBody ret) >>= fromVariant
    maybeToEither "Failed to parse introspection XML" $
      I.parseXML "/" xmlText

firstMethodReturn :: Either MethodError MethodReturn -> Either String MethodReturn
firstMethodReturn (Left e) = Left (formatErrorName (methodErrorName e))
firstMethodReturn (Right r) = Right r

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e = maybe (Left e) Right

buildAndRegisterInterface :: Client -> BusName -> ObjectPath -> ObjectPath -> I.Interface -> IO ()
buildAndRegisterInterface client busName pathToProxy registrationPath iIface = do
  let iface = buildInterface client busName pathToProxy iIface
  export client registrationPath iface
  forwardSignals client busName pathToProxy registrationPath (I.interfaceName iIface)

buildInterface :: Client -> BusName -> ObjectPath -> I.Interface -> Interface
buildInterface client busName pathToProxy I.Interface
  { I.interfaceName = name
  , I.interfaceMethods = methods
  , I.interfaceSignals = signals
  , I.interfaceProperties = properties
  } =
  Interface
    { interfaceName = name
    , interfaceMethods = map (buildMethod client busName pathToProxy name) methods
    , interfaceProperties = map (buildProperty client busName pathToProxy name) properties
    , interfaceSignals = signals
    }

buildMethod :: Client -> BusName -> ObjectPath -> InterfaceName -> I.Method -> Method
buildMethod client busName pathToProxy _ifaceName introspectionMethod =
  Method
    { methodName = I.methodName introspectionMethod
    , inSignature = signature_ inTypes
    , outSignature = signature_ outTypes
    , methodHandler = lift . handler
    }
  where
    args = I.methodArgs introspectionMethod
    inTypes = [I.methodArgType a | a <- args, I.methodArgDirection a == I.In]
    outTypes = [I.methodArgType a | a <- args, I.methodArgDirection a == I.Out]
    handler theMethodCall =
      buildReply <$> call client
        theMethodCall
          { methodCallPath = pathToProxy
          , methodCallDestination = Just busName
          }

buildReply :: Either MethodError MethodReturn -> Reply
buildReply (Left e) = ReplyError (methodErrorName e) (methodErrorBody e)
buildReply (Right r) = ReplyReturn (methodReturnBody r)

buildProperty :: Client -> BusName -> ObjectPath -> InterfaceName -> I.Property -> Property
buildProperty client busName pathToProxy ifaceName introspectionProperty =
  Property
    { propertyName = memberName_ (I.propertyName introspectionProperty)
    , propertyType = I.propertyType introspectionProperty
    , propertyGetter =
        if I.propertyRead introspectionProperty
          then Just getter
          else Nothing
    , propertySetter =
        if I.propertyWrite introspectionProperty
          then Just setter
          else Nothing
    }
  where
    propName = memberName_ (I.propertyName introspectionProperty)
    baseCall =
      (methodCall pathToProxy ifaceName propName)
        { methodCallDestination = Just busName
        }
    getter = either (const $ toVariant ("" :: String)) id <$> getProperty client baseCall
    setter v = void $ setProperty client baseCall v

forwardSignals :: Client -> BusName -> ObjectPath -> ObjectPath -> InterfaceName -> IO ()
forwardSignals client busName pathToProxy registrationPath ifaceName = do
  mOwner <- getNameOwner client busName
  case mOwner of
    Nothing -> logM "DBus.Proxy" WARNING $ "Could not resolve name owner for " <> formatBusName busName
    Just owner -> do
      let forwardSignal sig =
            -- Rewrite the path if the exported proxy lives at a different path.
            emit client $ if signalPath sig == pathToProxy
              then sig { signalPath = registrationPath }
              else sig
          matchRule =
            matchAny
              { matchPath = Just pathToProxy
              , matchInterface = Just ifaceName
              , matchSender = Just owner
              }
      void $ addMatch client matchRule forwardSignal

getNameOwner :: Client -> BusName -> IO (Maybe BusName)
getNameOwner client name = do
  let callMsg =
        (methodCall dbusPath (interfaceName_ "org.freedesktop.DBus") (memberName_ "GetNameOwner"))
          { methodCallDestination = Just dbusName
          , methodCallBody = [toVariant (formatBusName name)]
          }
  reply <- call client callMsg
  case reply of
    Left _ -> pure Nothing
    Right ret ->
      case listToMaybe (methodReturnBody ret) >>= fromVariant of
        Nothing -> pure Nothing
        Just (ownerStr :: String) ->
          -- Unique name like ":1.123".
          (Just <$> parseBusName ownerStr) `catch` \(_ :: SomeException) -> pure Nothing
