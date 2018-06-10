{-# LANGUAGE CPP #-}
module Pure.Animation (addAnimation,addAnimations,addAnimationsReverse) where

import Control.Concurrent (MVar,newEmptyMVar,forkIO,takeMVar,putMVar,tryPutMVar)
import Control.Monad (void,forever)
import Data.IORef (IORef,newIORef,atomicModifyIORef')
import System.IO.Unsafe (unsafePerformIO)

-- from ghcjs-base
#ifdef __GHCJS__
import GHCJS.Foreign.Callback (syncCallback1,OnBlocked(..),releaseCallback)
#endif

-- from pure-core (local)
import Pure.Data.Lifted (requestAnimationFrame)

-- TODO: Is there a way to do this without the MVar
--       and without calling requestAnimationFrame
--       continuously?

{-# NOINLINE addAnimation #-}
addAnimation :: IO () -> IO Bool
addAnimation a = animator `seq` do
    atomicModifyIORef' animationQueue $ \as -> (a:as,())
    tryPutMVar animationsAwaiting ()

{-# NOINLINE addAnimations #-}
addAnimations :: [IO ()] -> IO Bool
addAnimations new = animator `seq` do
    atomicModifyIORef' animationQueue $ \as -> (reverse new ++ as,())
    tryPutMVar animationsAwaiting ()

{-# NOINLINE addAnimationsReverse #-}
addAnimationsReverse :: [IO ()] -> IO Bool
addAnimationsReverse new = animator `seq` do
    atomicModifyIORef' animationQueue $ \as -> (new ++ as,())
    tryPutMVar animationsAwaiting ()

{-# NOINLINE animationsAwaiting #-}
animationsAwaiting :: MVar ()
animationsAwaiting = unsafePerformIO newEmptyMVar

{-# NOINLINE animationQueue #-}
animationQueue :: IORef [IO ()]
animationQueue = unsafePerformIO (newIORef [])

{-# NOINLINE animator #-}
animator :: ()
animator = unsafePerformIO $ void $ forkIO await
  where
    await :: IO ()
    await = forever $ do
        takeMVar animationsAwaiting
        as <- atomicModifyIORef' animationQueue $ \as -> ([],as)
        animate as

{-# INLINE (<<) #-}
(<<) :: IO b -> IO a -> IO b
(<<) = flip (>>)

animate :: [IO ()] -> IO ()
animate as = do
#ifdef __GHCJS__
    barrier <- newEmptyMVar
    callback <- syncCallback1 ContinueAsync $ \_ -> do
      run as
      putMVar barrier ()
    requestAnimationFrame callback
    takeMVar barrier
    releaseCallback callback
#else
    run as
#endif
  where
    {-# INLINE run #-}
    run = foldr (<<) (return ())



