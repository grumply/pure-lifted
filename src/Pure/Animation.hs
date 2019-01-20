{-# LANGUAGE CPP, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-full-laziness -fno-state-hack #-}
module Animation (addAnimation,addAnimations,addAnimationsReverse) where

import Control.Exception (SomeException,catch)
import Control.Concurrent (MVar,newEmptyMVar,forkIO,takeMVar,putMVar,tryPutMVar)
import Control.Monad (void,forever)
import Data.Function (fix)
import Data.IORef (IORef,newIORef,atomicModifyIORef')
import System.IO.Unsafe (unsafePerformIO)

import Pure.Data.Txt (Txt,toTxt)

-- from ghcjs-base
#ifdef __GHCJS__
import GHCJS.Foreign.Callback (Callback,syncCallback1,OnBlocked(..),releaseCallback)
import GHCJS.Concurrent
#endif

-- from pure-core (local)
import Pure.Data.Lifted (JSV)

-- TODO: Is there a way to do this without the MVar
--       and without calling requestAnimationFrame
--       continuously?
{-# INLINE addAnimation #-}
addAnimation :: IO () -> IO Bool
addAnimation a = animator `seq` do
    atomicModifyIORef' animationQueue $ \as -> (a:as,())
    tryPutMVar animationsAwaiting ()

{-# INLINE addAnimations #-}
addAnimations :: [IO ()] -> IO Bool
addAnimations new = animator `seq` do
    atomicModifyIORef' animationQueue $ \as -> (reverse new ++ as,())
    tryPutMVar animationsAwaiting ()

{-# INLINE addAnimationsReverse #-}
addAnimationsReverse :: [IO ()] -> IO Bool
addAnimationsReverse new = animator `seq` do
    atomicModifyIORef' animationQueue $ \as -> (new ++ as,())
    b <- tryPutMVar animationsAwaiting ()
    return b

{-# NOINLINE animationsAwaiting #-}
animationsAwaiting :: MVar ()
animationsAwaiting = unsafePerformIO newEmptyMVar

{-# NOINLINE animationQueue #-}
animationQueue :: IORef [IO ()]
animationQueue = unsafePerformIO (newIORef [])

{-# NOINLINE animator #-}
animator :: ()
animator = unsafePerformIO $ void $ forkIO (fix (work >>))
  where
    work = (fix (await >>)) `catch` \(_ :: SomeException) -> return ()
      where
        await :: IO ()
        await = do
          takeMVar animationsAwaiting
          as <- atomicModifyIORef' animationQueue $ \as -> ([],as)
          animate (reverse as)

animate :: [IO ()] -> IO ()
animate as = do
#ifdef __GHCJS__
    barrier <- newEmptyMVar
    let work _ = synchronously $ sequence_ as >>= putMVar barrier
    callback <- syncCallback1 ContinueAsync work
    requestAnimation callback
    takeMVar barrier
    releaseCallback callback
#else
    sequence_ as
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
  "$r = window.requestAnimationFrame($1)" request_animation_frame_js :: Callback (JSV -> IO ()) -> IO JSV

{-# INLINE requestAnimation #-}
requestAnimation :: Callback (JSV -> IO ()) -> IO ()
requestAnimation cb = void $ request_animation_frame_js cb
#endif
