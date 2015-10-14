{-# LANGUAGE MagicHash, UnboxedTuples, GHCForeignImportPrim, UnliftedFFITypes, BangPatterns #-}
module UnsafeSetField where

import GHC.Exts

foreign import prim "unsafeSetFieldzh" unsafeSetField#
  :: Int# -> Any -> Any -> (##)

unsafeSetField :: Int -> a -> b -> IO ()
unsafeSetField (I# i) !x y =
  case unsafeSetField# i (unsafeCoerce# x :: Any) (unsafeCoerce# y :: Any) of
    (##) -> return ()
{-# INLINEABLE unsafeSetField #-}

unsetField :: a
unsetField = error "Unset field accessed too early"
