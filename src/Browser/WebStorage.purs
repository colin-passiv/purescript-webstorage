module Browser.WebStorage
  ( class Storage
  , LocalStorage(..)
  , StorageKey(..)
  , SessionStorage(..)
  , localStorage
  , sessionStorage
  , clear
  , getItem
  , key
  , length
  , removeItem
  , setItem
  ) where


import Prelude

import Effect (Effect)
import Data.Function.Uncurried (Fn3, Fn2, runFn3, runFn2)
import Data.Maybe (Maybe(..))

newtype StorageKey = StorageKey String

class Storage s where
    clear :: s -> Effect Unit
    getItem :: s -> StorageKey -> Effect (Maybe String)
    key :: s -> Int -> Effect (Maybe StorageKey)
    length :: s -> Effect Int
    removeItem :: s -> StorageKey -> Effect Unit
    setItem :: s -> StorageKey -> String -> Effect Unit

instance storageLocalStorage :: Storage LocalStorage where
    length _ = unsafeLength localStorage
    key _ n = runFn3 unsafeKey (null2Maybe StorageKey) localStorage n
    getItem _ k = runFn3 unsafeGetItem (null2Maybe identity) localStorage k
    setItem _ k v = runFn3 unsafeSetItem localStorage k v
    removeItem _ k = runFn2 unsafeRemoveItem localStorage k
    clear _ = unsafeClear localStorage

instance storageSessionStorage :: Storage SessionStorage where
    length _ = unsafeLength sessionStorage
    key _ n = runFn3 unsafeKey (null2Maybe StorageKey) sessionStorage n
    getItem _ k = runFn3 unsafeGetItem (null2Maybe identity) sessionStorage k
    setItem _ k v = runFn3 unsafeSetItem sessionStorage k v
    removeItem _ k = runFn2 unsafeRemoveItem sessionStorage k
    clear _ = unsafeClear sessionStorage

foreign import data LocalStorage :: Type
foreign import data SessionStorage :: Type

foreign import localStorage :: LocalStorage
foreign import sessionStorage :: SessionStorage

foreign import unsafeLength
    :: forall storage. storage -> Effect Int

foreign import unsafeKey
    :: forall storage. Fn3 (String -> Maybe StorageKey) storage Int (Effect (Maybe StorageKey))

foreign import unsafeGetItem
    :: forall storage. Fn3 (String -> Maybe String) storage StorageKey (Effect (Maybe String))

foreign import unsafeSetItem
    :: forall storage. Fn3 storage StorageKey String (Effect Unit)

foreign import unsafeRemoveItem
    :: forall storage. Fn2 storage StorageKey (Effect Unit)

foreign import unsafeClear
    :: forall storage. storage -> Effect Unit

foreign import null2MaybeImpl
    :: forall a b. Fn3 (a -> Maybe b) (Maybe b) a (Maybe b)

null2Maybe :: forall a b. (a -> b) -> a -> Maybe b
null2Maybe f n = runFn3 null2MaybeImpl (f >>> Just) Nothing n
