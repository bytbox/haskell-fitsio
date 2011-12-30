{-# LANGUAGE ForeignFunctionInterface #-}

module Data.Fits.WCS where

import C2HS

fk425e :: Float -> Float -> Float -> (Float, Float)
fk425e a1 a2 a3 =
  unsafePerformIO $
  withFloatConv a1 $ \a1' -> 
  withFloatConv a2 $ \a2' -> 
  let {a3' = cFloatConv a3} in 
  let {res = fk425e'_ a1' a2' a3'} in
  peekFloatConv a1'>>= \a1'' -> 
  peekFloatConv a2'>>= \a2'' -> 
  return (a1'', a2'')
{-# LINE 12 "WCS.chs" #-}

fk524e :: Float -> Float -> Float -> (Float, Float)
fk524e a1 a2 a3 =
  unsafePerformIO $
  withFloatConv a1 $ \a1' -> 
  withFloatConv a2 $ \a2' -> 
  let {a3' = cFloatConv a3} in 
  let {res = fk524e'_ a1' a2' a3'} in
  peekFloatConv a1'>>= \a1'' -> 
  peekFloatConv a2'>>= \a2'' -> 
  return (a1'', a2'')
{-# LINE 19 "WCS.chs" #-}

foreign import ccall unsafe "WCS.chs.h fk425e"
  fk425e'_ :: ((Ptr CDouble) -> ((Ptr CDouble) -> (CDouble -> ())))

foreign import ccall unsafe "WCS.chs.h fk524e"
  fk524e'_ :: ((Ptr CDouble) -> ((Ptr CDouble) -> (CDouble -> ())))
