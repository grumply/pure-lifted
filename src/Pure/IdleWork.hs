{-# LANGUAGE CPP #-}
module Pure.IdleWork (addIdleWork) where

import Control.Concurrent (MVar,newEmptyMVar,forkIO,takeMVar,putMVar,tryPutMVar,yield)
import Control.Monad (void)
import Data.IORef (IORef,newIORef,atomicModifyIORef')
import System.IO.Unsafe (unsafePerformIO)

-- from ghcjs-base
#ifdef __GHCJS__
import GHCJS.Foreign.Callback (syncCallback1,OnBlocked(..),releaseCallback)
#endif

-- from pure-core (local)
import Pure.Data.Lifted (requestIdleCallback)

{-# NOINLINE addIdleWork #-}
addIdleWork :: IO () -> IO Bool
addIdleWork a = idleWorker `seq` do
    atomicModifyIORef' idleWorkQueue $ \as -> (a:as,())
    tryPutMVar idleWorkAwaiting ()

{-# NOINLINE idleWorkAwaiting #-}
idleWorkAwaiting :: MVar ()
idleWorkAwaiting = unsafePerformIO newEmptyMVar

{-# NOINLINE idleWorkQueue #-}
idleWorkQueue :: IORef [IO ()]
idleWorkQueue = unsafePerformIO (newIORef [])

{-# NOINLINE idleWorker #-}
idleWorker :: ()
idleWorker = unsafePerformIO $ void $ forkIO await
  where
    await :: IO ()
    await = do
        yield
        takeMVar idleWorkAwaiting
        as <- atomicModifyIORef' idleWorkQueue $ \as -> ([],as)
        workIdly as

    {-# INLINE (<<) #-}
    (<<) :: IO b -> IO a -> IO b
    (<<) = flip (>>)

    workIdly :: [IO ()] -> IO ()
    workIdly [] = await
    workIdly as = await << do
#ifdef __GHCJS__
        barrier <- newEmptyMVar
        callback <- syncCallback1 ContinueAsync $ \_ -> do
          run as
          putMVar barrier ()
        requestIdleCallback callback
        takeMVar barrier
        releaseCallback callback
#else
        run as
#endif
      where
        {-# INLINE run #-}
        run as = do
          -- TODO: figure out if bs is ever (/= [])
          bs <- atomicModifyIORef' idleWorkQueue $ \bs -> ([],bs)
          sequencer as
          sequencer bs
          where
            {-# INLINE sequencer #-}
            sequencer :: [IO ()] -> IO ()
            sequencer = foldr (<<) (return ())



