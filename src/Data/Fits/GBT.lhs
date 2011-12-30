> {-# LANGUAGE PArr #-}

Specializes FQL with knowledge of GBT FITS files.  In particular, it
is aware of the GBT project directory structure, scans and
instruments, ScanLog directory, and GO FITS files.

> module Data.Fits.GBT where

> import Control.Monad.Reader
> import Data.Char
> import Data.Fits.FitsIO
> import Data.Fits.FQL
> import Data.List
> import Data.Maybe
> import System.FilePath

> data GBTEnv = GBTEnv {
>     gbtDir   :: FilePath
>   , gbtScans :: [(Int, [(String, FilePath)])]
>   , gbtFits  :: Maybe FitsFile
>   }

> instance FQLEnv GBTEnv where
>     fitsFile = fromJust . gbtFits

> gbtEnv      :: FilePath -> GBTEnv
> gbtEnv root = GBTEnv { gbtDir = root, gbtScans = [], gbtFits = Nothing }

The GBT monad is an FQL monad using the GBT environment.

> type GBT t = FQL GBTEnv t

> with              :: String -> GBT t -> GBT t
> with instrument f = do
>     Just path <- asks (filePath instrument)
>     fitsFile  <- lift $ openDiskFile path ReadOnly
>     status    <- lift $ getStatus
>     if status == noError
>       then do
>         result <- local (\env -> env { gbtFits = Just fitsFile }) f
>         -- closeFile fitsFile
>         return result
>       else return undefined

> filePath          :: String -> GBTEnv -> Maybe FilePath
> filePath name env = fmap (buildPath env) .
>     find (\(instrument, _) -> instrument `leq` name) . scanFiles $ env

> buildPath                 :: GBTEnv -> (String, FilePath) -> FilePath
> buildPath env (dir, file) = gbtDir env </> dir </> file

Returns the set of files corresponding to the current scan.

> scanFiles                             :: GBTEnv -> [(String, String)]
> scanFiles GBTEnv { gbtScans = []    } = []
> scanFiles GBTEnv { gbtScans = scans } = snd . head $ scans

Case-insensitive string equality.

> leq             :: String -> String -> Bool
> str1 `leq` str2 = lowercase str1 == lowercase str2
>   where
>     lowercase = map toLower
