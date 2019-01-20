{-# LANGUAGE CPP, BangPatterns, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-full-laziness -fno-state-hack #-}
module Pure.IdleWork (addIdleWork, addIdleWorks, addIdleWorksReverse) where

import Control.Concurrent (MVar,newEmptyMVar,forkIO,takeMVar,putMVar,tryPutMVar,yield,threadDelay)
import Control.Monad (forever,join,void)
import Data.Foldable
import Data.Function (fix)
import Data.IORef (IORef,newIORef,readIORef,writeIORef,atomicModifyIORef')
import Data.List (intersperse)
import System.IO.Unsafe (unsafePerformIO)

import Pure.Data.Lifted (JSV)

-- from ghcjs-base
#ifdef __GHCJS__
import GHCJS.Foreign.Callback (syncCallback1,OnBlocked(..),releaseCallback)
import GHCJS.Concurrent (synchronously)
#endif

-- from pure-core (local)
import Pure.Data.Lifted (requestIdleCallback)

import Control.Exception (SomeException,catch)
import Pure.Data.Txt (Txt,toTxt)

-- |
-- GHCJS: We rely on a poly-filled version of requestIdleCallback
-- which uses the browser's rIC if it is available or a setTimeout(_,1)
-- if rIC is not available. Access to the rIC timeRemaining is not given,
-- but we check if a reschedule is necessary between each action and act
-- accordingly, cleaning up callbacks as we go.
--
-- GHC: All actions are run in a forked thread that yields often.
--
{-# INLINE addIdleWork #-}
addIdleWork :: IO () -> IO Bool
addIdleWork a = idleWorker `seq` do
    atomicModifyIORef' idleWorkQueue $ \as -> (a:as,())
    tryPutMVar idleWorkAwaiting ()

{-# INLINE addIdleWorks #-}
addIdleWorks :: [IO ()] -> IO Bool
addIdleWorks new = idleWorker `seq` do
    atomicModifyIORef' idleWorkQueue $ \as -> (reverse new ++ as,())
    tryPutMVar idleWorkAwaiting ()

{-# INLINE addIdleWorksReverse #-}
addIdleWorksReverse :: [IO ()] -> IO Bool
addIdleWorksReverse new = idleWorker `seq` do
    atomicModifyIORef' idleWorkQueue $ \as -> (new ++ as,())
    tryPutMVar idleWorkAwaiting ()

{-# NOINLINE idleWorkAwaiting #-}
idleWorkAwaiting :: MVar ()
idleWorkAwaiting = unsafePerformIO newEmptyMVar

{-# NOINLINE idleWorkQueue #-}
idleWorkQueue :: IORef [IO ()]
idleWorkQueue = unsafePerformIO (newIORef [])

{-# NOINLINE idleWorker #-}
idleWorker :: ()
idleWorker = unsafePerformIO $ void $ forkIO (fix (work >>))
  where
    work = (fix (await >>)) `catch` \(_ :: SomeException) -> return ()
      where
        await :: IO ()
        await = do
          takeMVar idleWorkAwaiting
          as <- atomicModifyIORef' idleWorkQueue $ \as -> ([],as)
          workIdly (reverse as)

workIdly :: [IO ()] -> IO ()
workIdly as = do
#ifndef __GHCJS__
    yield
    sequence_ (intersperse yield as)
#else
    wrapper >>= traverse_ workIdly
  where
    wrapper = do
      barrier <- newEmptyMVar
      let work deadline = synchronously $ worker deadline as >>= putMVar barrier
      callback <- syncCallback1 ContinueAsync work
      requestIdleCallback callback
      result <- takeMVar barrier
      releaseCallback callback
      return result
      where
        worker deadline = go
          where

            go [] = return Nothing

            go (a:as) = do
              a `catch` \(_ :: SomeException) -> return () -- ignore failed idle workers
              tr <- hasTimeRemaining deadline
              let k = if tr then go else return . Just
              k as
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
  "var tr = $1.timeRemaining(); $r = tr > 0;" hasTimeRemaining :: JSV -> IO Bool
#endif
