{-# LANGUAGE CPP, BangPatterns #-}
module Pure.IdleWork (addIdleWork,addIdleWorks,addIdleWorksReverse) where

import Control.Concurrent (MVar,newEmptyMVar,forkIO,takeMVar,putMVar,tryPutMVar,yield)
import Control.Monad (forever,join,void)
import Data.Function (fix)
import Data.IORef (IORef,newIORef,readIORef,writeIORef,atomicModifyIORef')
import Data.List (intersperse)
import System.IO.Unsafe (unsafePerformIO)

import Pure.Data.Lifted (JSV)

-- from ghcjs-base
#ifdef __GHCJS__
import GHCJS.Foreign.Callback (syncCallback1,OnBlocked(..),releaseCallback)
#else
import System.CPUTime
#endif

-- from pure-core (local)
import Pure.Data.Lifted (requestIdleCallback)

-- |
-- GHCJS: We rely on a poly-filled version of requestIdleCallback
-- which uses the browser's rIC if it is available or a setTimeout(_,1)
-- if rIC is not available. Access to the rIC timeRemaining is not given,
-- but we check if a reschedule is necessary between each action and act
-- accordingly, cleaning up callbacks as we go.
--
-- GHC: We try to exploit the round-robin nature of thread scheduling
-- to detect and exploit idle time. On entry we take the current time
-- and immediately yield. On re-entry, we take the time and compare it
-- with the initial entry time. If the entry and re-entry time differ
-- by less than 1 millisecond, we perform one action and then start over.
-- If it is more than one millisecond, we start over. If 1000 iterations
-- pass without there being a detectable idle time, we assume load is high
-- and simply pile on to try to avoid being the source of a memory leak.
-- In the worst case, this should be /approximately/ the same as not calling
-- addIdleWork. In the best case, it will exploit idle time and improve
-- server response times.
--
-- Best practices:
--   1. Keep cross-thread resource sharing to a minimum
--   2. Avoid thread synchronization with the idle worker via MVar barriers
--   3. Avoid using addIdleWork for loads with ACID-like expectations
--
addIdleWork :: IO () -> IO Bool
addIdleWork a = idleWorker `seq` do
    atomicModifyIORef' idleWorkQueue $ \as -> (a:as,())
    tryPutMVar idleWorkAwaiting ()
{-# NOINLINE addIdleWork #-}

addIdleWorks :: [IO ()] -> IO Bool
addIdleWorks new = idleWorker `seq` do
    atomicModifyIORef' idleWorkQueue $ \as -> (reverse new ++ as,())
    tryPutMVar idleWorkAwaiting ()
{-# NOINLINE addIdleWorks #-}

addIdleWorksReverse :: [IO ()] -> IO Bool
addIdleWorksReverse new = idleWorker `seq` do
    atomicModifyIORef' idleWorkQueue $ \as -> (new ++ as,())
    tryPutMVar idleWorkAwaiting ()
{-# NOINLINE addIdleWorksReverse #-}

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
    await = forever $ do
        takeMVar idleWorkAwaiting
        as <- atomicModifyIORef' idleWorkQueue $ \as -> ([],as)
        workIdly (reverse as)

workIdly :: [IO ()] -> IO ()
#ifdef __GHCJS__
workIdly as = do
    barrier <- newEmptyMVar
    go barrier as
    join (takeMVar barrier)
  where
    go :: MVar (IO ()) -> [IO ()] -> IO ()
    go barrier = go'
      where
        go' :: [IO ()] -> IO ()
        go' as0 = do
          cb <- newIORef undefined
          callback <- syncCallback1 ContinueAsync $ \deadline -> flip fix as0 $ \continue as0 ->
            case as0 of
              [] -> do
                callback <- readIORef cb
                putMVar barrier (releaseCallback callback)
              [a] -> a >> continue []
              (a0:a1:as) -> do
                !_  <- a0
                !tr <- hasTimeRemaining deadline
                if tr
                  then continue (a1:as)
                  else do
                    let a = do
                          callback <- readIORef cb
                          releaseCallback callback
                          a1
                    go' (a:as)
          writeIORef cb callback
          void $ requestIdleCallback callback
#else
workIdly = go 0
  where
    go !_ [] = return ()
    go n as0@(a:as)
      -- avoid hogging CPU by yielding as often as possible
      | n == 1000 = foldr (>>) (return ()) $ intersperse yield as0
      | otherwise = do
        !start <- getCPUTime
        yield
        !rentry <- getCPUTime
        if rentry - start < 1000000000 -- 1 ms
          then do
            !_ <- a
            go 0 as
          else go (n + 1) as0
#endif



#ifdef __GHCJS__
foreign import javascript unsafe
  "var tr = $1.timeRemaining(); $r = tr > 0;" hasTimeRemaining :: JSV -> IO Bool
#endif
