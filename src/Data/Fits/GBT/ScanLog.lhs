> {-# LANGUAGE PArr #-}

Encapsulates knowledge of GBT ScanLogs.

> module Data.Fits.GBT.ScanLog where

> import Control.Monad.Reader
> import Data.Fits.FitsIO
> import Data.Fits.FQL
> import Data.Fits.GBT
> import Data.List
> import GHC.PArr
> import System.FilePath

> gbtProject      :: FilePath -> IO GBTEnv
> gbtProject root = runFits . runFQL (gbtEnv root) $ readScanLog
  
> readScanLog :: GBT GBTEnv
> readScanLog = ask >>= \env -> do
>     fitsFile <- lift $ openDiskFile (gbtDir env </> "ScanLog.fits") ReadOnly
>     local (\env -> env { gbtFits = Just fitsFile }) $ do
>       namedHDU "SCANLOG"
>       scan     <- column' $ "SCAN"
>       filePath <- column' $ "FILEPATH"
>       -- fitsCloseFile fitsFile
>       return env { gbtScans = groupScans scan filePath }

> groupScans             :: [Int] -> [String] -> [(Int, [(String, String)])]
> groupScans scans paths = let
>     raw    = groupBy (\(x, _) (y, _) -> x == y) $ zip scans paths
>     cooked = [(head xs, ys) | (xs, ys) <- map unzip raw]
>     in [(scan, [(dir, file) | path <- paths
>                             , any isPathSeparator path
>                             , (file : dir : _) <- [reverse . splitPath $ path]])
>                             | (scan, paths) <- cooked]
