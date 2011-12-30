> {-# LANGUAGE PArr #-}

Encapsulates knowledge of GBT Antenna FITS.

> module Data.Fits.GBT.Antenna where

> import Data.Fits.FitsIO
> import Data.Fits.FQL
> import Data.Fits.GBT
> import GHC.PArr

> columns       :: FitsValue a => [String] -> GBT [[:a:]]
> columns names = with "ANTENNA" $ do
>     namedHDU "ANTPOSGR" <|> namedHDU "ANTPOSPF"
>     mapM column $ names
