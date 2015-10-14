{-# LANGUAGE ScopedTypeVariables, RecursiveDo #-}
--import GHC.Vis
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
--import Criterion
--import Criterion.Main

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


variants =
    [ ("accumAppend", accumAppend)
    , ("accumAppendSeq", accumAppendSeq)
    , ("accumReverse", accumReverse)
    , ("recursion", listRecurse)
    , ("replicateM", listReplicateM)
    , ("accumDList", accumDList)
    , ("streams", listStreams)
    , ("unsafeInterleave", listUnsafeInterleave)
    , ("listFix", listFix)
    ]


main = do
    args <- getArgs
    let vars = case args of 
            []  -> variants
            l -> map (\n -> fromJust $ find ((==n).fst) variants ) l
    putStrLn "Stack checks"
    performGC
    forM_ vars $ \(n, f) -> do
        putStr (n ++ ": ")
        thread <- async $ catch
            (do l <- f 10000
                l `deepseq` return ()
                return True
            ) (\(e:: AsyncException ) -> return False)
        ok <- wait thread
        if ok then putStrLn "ok"
              else putStrLn "not ok"
        performGC
    {-
    defaultMain [ bench n $ nfIO (f 1000) | (n,f) <- variants]
    -}
    {-
    l <- accumDList 5
    vis
    switch
    view l "l"
    export "dlist.svg"
    -}
