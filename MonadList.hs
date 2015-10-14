{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# LANGUAGE RecursiveDo #-}
module MonadList where

import Control.Monad
import Control.Exception
import System.IO.Unsafe
import Data.Functor
import qualified Data.Vector.Fusion.Stream.Monadic as MStream
import Control.DeepSeq
import System.Mem
import System.Environment
import Data.Maybe
import Data.List
import Control.Concurrent.Async

import GHC.IO

import UnsafeSetField
import Holes

getInput :: IO Int
getInput = {- putStr "" >> -} return 42
{-# NOINLINE getInput #-}

listReplicateM :: Int -> IO [Int]
listReplicateM n = Control.Monad.replicateM n getInput
{-# NOINLINE listReplicateM #-}

listRecurse :: Int -> IO [Int]
listRecurse n = go n
  where
    go 0 = return []
    go n = do {x <- getInput; r <- go (n-1); return (x:r)}
{-# NOINLINE listRecurse #-}

listUnsafeInterleave :: Int -> IO [Int]
listUnsafeInterleave n = do {l <- go n; return $!! l}
  where
    go 0 = return []
    go n = do x <- getInput
              r <- unsafeInterleaveIO (go (n-1))
              return (x:r)
{-# NOINLINE listUnsafeInterleave #-}

listFix :: Int -> IO [Int]
listFix n = go n
  where
    go 0 = return []
    go n = mdo x <- getInput
               r' <- return $ x:r
               r <- go (n-1)
               return r'

accumAppend :: Int -> IO [Int]
accumAppend n = go n []
  where
    go 0 l = return l
    go n l = do {x <- getInput; go (n-1) (l++[x])}
{-# NOINLINE accumAppend #-}
    
accumAppendSeq :: Int -> IO [Int]
accumAppendSeq n = go n []
  where
    go 0 l = return l
    go n l = do {x <- getInput ; go (n-1) $!! (l++[x])}
{-# NOINLINE accumAppendSeq #-}
    
accumReverse :: Int -> IO [Int]
accumReverse n = go n []
  where
    go 0 l = return (reverse l)
    go n l = do {x <- getInput ; go (n-1) (x:l)}
{-# NOINLINE accumReverse #-}
    
accumDList :: Int -> IO [Int]
accumDList n = go n id
  where
    go 0 l = return (l [])
    go n l = do {x <- getInput; go (n-1) (l . (x:))}
{-# NOINLINE accumDList #-}
    
listStreams :: Int -> IO [Int]
listStreams n = MStream.toList $ MStream.replicateM n getInput
{-# NOINLINE listStreams #-}

-- Adapted from http://neilmitchell.blogspot.co.uk/2015/09/making-sequencemapm-for-io-take-o1-stack.html
escapeIO :: Int -> IO [Int]
escapeIO n = do
        ys <- IO $ \r -> (# r, apply r n #)
        evaluate $ demand ys
        return ys
    where
        IO getInput' = getInput

        apply r 0 = []
        apply r n = case getInput' r of
            (# r, y #) -> y : apply r (n-1)

        demand [] = ()
        demand (x:xs) = demand xs

-- Adapted from http://www.twanvl.nl/blog/haskell/unsafe-sequence
hackIO :: Int -> IO [Int]
hackIO 0 = return []
hackIO n = do
    x0 <- getInput
    let front = x0:unsetField
    go front (n-1)
    return front
  where
    go back 0 =
        unsafeSetField 1 back []
    go back n = do
        x <- getInput
        let back' = x:unsetField
        unsafeSetField 1 back back'
        go back' (n-1)
{-# NOINLINE hackIO #-}

holeIO :: Int -> IO [Int]
holeIO n = do
    front <- newHole
    go front n
    return front
  where
    go back 0 =
        setHole back []
    go back n = do
        x <- getInput
        back' <- newHole
        setHole back (x:back')
        go back' (n-1)
{-# NOINLINE holeIO #-}


variants :: [(String, Int -> IO [Int])]
variants =
    [{- ("accumAppend", accumAppend)
    ,   ("accumAppendSeq", accumAppendSeq)
    ,-} ("accumReverse", accumReverse)
    ,   ("recursion", listRecurse)
    ,   ("replicateM", listReplicateM)
    ,   ("accumDList", accumDList)
    ,   ("streams", listStreams)
    ,   ("unsafeInterleave", listUnsafeInterleave)
    ,   ("listFix", listFix)
    ,   ("escapeIO", escapeIO)
    ,   ("hackIO", hackIO)
    ,   ("holeIO", holeIO)
    ]
