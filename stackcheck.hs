{-# LANGUAGE ScopedTypeVariables #-}

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

import MonadList

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
