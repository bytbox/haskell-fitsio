> {-# LANGUAGE PArr #-}

This module brings the FITS Query Language (FQL) to Haskell.  Where
FitsIO is a low-level interface to data stored in FITS files---it's
just a wrapper around the CFITSIO library---FQL attempts to provide a
high-level interface that abstracts away much of the tedium.

> module Data.Fits.FQL where

> import Control.Monad.Reader
> import Data.Fits.FitsIO
> import GHC.PArr

The FQL monad couples FitsIO with a simple computational environment.

> type FQL env t = ReaderT env FitsIO t

> runFQL       :: env -> FQL env t -> FitsIO t
> runFQL env f = runReaderT f env

We use a typeclass (rather than an ADT) for FQLEnv so that consumers
and more easily provide their own environment.  So long as that
environment provides access to a FitsFile, we're good to go.

> class FQLEnv env where
>     fitsFile   :: env -> FitsFile

> instance FQLEnv FitsFile where
>     fitsFile f = f

> fits :: FQLEnv env => FQL env FitsFile
> fits = asks fitsFile

The "or" combinator here is intended to be any easy way to combine FQL
operations that might fail.  As a simple example, some GBT antenna
files are gregorian and others are prime-focus.  An easy way to handle
both is:

namedHDU "ANTPOSGR" <|> namedHDU "ANTPOSPF"

> (<|>)   :: FQLEnv env => FQL env t -> FQL env t -> FQL env t
> f <|> g = ReaderT $ \r -> FitsIO $ \s ->
>     if s == noError
>       then runFitsIO (runFQL r f) s >>= \(s', x) ->
>         if s' == noError
>           then return (s', x)
>           else runFitsIO (runFQL r g) noError
>       else return (s, undefined)

This should be migrated to FitsIO as an ADT, but for now...

> casesen   = True
> caseinsen = False

Most of the time we don't really care about HDU types or versions, so
long as they contain the data we're looking for.

> data HDU = HDU {
>     hduName    :: String
>   , hduType    :: HduType
>   , hduVersion :: Int
>   }

> defaultHDU :: HDU
> defaultHDU = HDU { hduName = "", hduType = AnyHdu, hduVersion = 0 }

Move to the desired Header/Data Unit.
  
> openHDU     :: FQLEnv env => HDU -> FQL env ()
> openHDU hdu = fits >>= \f -> lift $ movNamHdu f typ name version
>   where
>     HDU { hduName = name, hduType = typ, hduVersion = version } = hdu

Move to the desired Header/Data Unit specified by name only.

> namedHDU      :: FQLEnv env => String -> FQL env ()
> namedHDU name = openHDU defaultHDU { hduName = name }

Read the full contents of the named column into a parallel array.

> column :: (FQLEnv env, FitsValue a) => String -> FQL env [:a:]
> column = fmap toP . column'

> column'      :: (FQLEnv env, FitsValue a) => String -> FQL env [a]
> column' name = fits >>= \f -> do
>     colNum  <- lift $ getColNum f caseinsen name
>     numRows <- lift $ getNumRows f
>     (xs, _) <- lift $ readCol f colNum 1 1 numRows readDef
>     return xs
