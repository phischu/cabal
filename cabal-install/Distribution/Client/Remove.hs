-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Remove
-- Copyright   :  (c) Philipp Schuster 2012
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Implementation of the 'cabal remove' command, which removes a user
-- specified package or optionally suggests packages for removal.
--
-----------------------------------------------------------------------------

module Distribution.Client.Remove (
    remove
  ) where

import Distribution.Simple.Compiler
         ( Compiler, PackageDBStack )
import Distribution.Simple.Program
         ( ProgramConfiguration )
import Distribution.Simple.Setup
        ( Flag(NoFlag,Flag), fromFlagOrDefault )
import Distribution.Client.Setup
        ( RemoveFlags(..) )
import Distribution.Simple.PackageIndex
        ( PackageIndex, deleteInstalledPackageId, allPackagesBySourcePackageId,
          reverseDependencyClosure )
import Distribution.Simple.Configure
        ( getInstalledPackages )
import Distribution.InstalledPackageInfo
         ( installedPackageId, timeStamp )
import Distribution.Package
        ( InstalledPackageId(..) )
import Distribution.Verbosity
        ( normal )
import Distribution.Simple.Utils
        ( die )

import Data.List
        ( sortBy, delete )
import Data.Ord
        ( comparing )

-- | The two things cabal remove can do:
-- remove a single package or remove duplicates
--
data RemovalAction = SinglePackage InstalledPackageId
                   | Duplicates
                   deriving Show

-- | Interpret the flags passed to 'cabal remove' to either
-- an error message or a 'RemovalAction'
--
interpretRemoveFlags :: RemoveFlags -> Either String RemovalAction
interpretRemoveFlags removeFlags = case singlePackage removeFlags of
    NoFlag    -> case duplicates removeFlags of
                     NoFlag    -> Left "Please specify at least either --package-id or --duplicates"
                     Flag True  -> Right Duplicates
                     Flag False -> Left "duplicates flag set to false"
    Flag ipid -> case duplicates removeFlags of
                     NoFlag    -> Right (SinglePackage ipid)
                     Flag True  -> Left "Please do not specify both --package-id and --duplicates"
                     Flag False -> Left "duplicates flag set to false"

-- | Given the mode of operation and a 'PackageIndex' find the
-- packages to be removed
--
removals :: RemovalAction -> PackageIndex -> [InstalledPackageId]
removals (SinglePackage ipid) _            = [ipid]
removals Duplicates           packageindex = findDuplicates packageindex []

-- | Collect unnecessary packages and delete them from the
-- 'PackageIndex' until there are no more
--
findDuplicates :: PackageIndex -> [InstalledPackageId] -> [InstalledPackageId]
findDuplicates packageIndex deletedPackages = case unnecessary packageIndex of
    [] -> deletedPackages
    ps -> findDuplicates packageIndex' (deletedPackages ++ ps) where
        packageIndex' = foldl (flip deleteInstalledPackageId) packageIndex ps

-- | All packages that are probably unnecessary in the 'PackageIndex'.
-- A package is unnecessary if no package depends on it and it is not
-- the latest instance of its version.
--
unnecessary :: PackageIndex -> [InstalledPackageId]
unnecessary packageIndex = concatMap
    (filter noReverseDependencies
    . map installedPackageId
    . tail
    . reverse
    . sortBy (comparing timeStamp)
    . snd)

    (allPackagesBySourcePackageId packageIndex)
    where
        noReverseDependencies ipid = null (delete ipid (map installedPackageId (reverseDependencyClosure packageIndex [ipid])))

-- | Actually remove a package. Currently only prints what it would
-- remove.
--
runRemoval :: Bool -> InstalledPackageId -> IO ()
runRemoval _ (InstalledPackageId ipid) = do
    print ("Would remove " ++ ipid)

-- | Stitch everything together.
--
remove :: PackageDBStack -> Compiler -> ProgramConfiguration -> RemoveFlags -> IO ()
remove packageDB compiler programConfiguration removeFlags = do
    let forreal        = fromFlagOrDefault False  (really removeFlags)
        verbosity      = fromFlagOrDefault normal (removeVerbosity removeFlags)
    packageIndex <- getInstalledPackages verbosity compiler packageDB programConfiguration
    removalAction <- either die return (interpretRemoveFlags removeFlags)
    mapM_ (runRemoval forreal) (removals removalAction packageIndex)

