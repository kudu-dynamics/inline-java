{-# LANGUAGE LambdaCase #-}

-- from new way
-- import Distribution.Simple.Setup
-- import Distribution.Verbosity
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Lazy as BL
-- import Control.Monad (unless)
-- import Data.Char (isSpace)
-- import Data.Functor ((<&>))
-- import Data.List (dropWhileEnd, union)
-- import Data.Maybe (fromJust, isJust)

import Distribution.Simple
import Distribution.System (Arch (..), OS (..), buildArch, buildOS)

import Distribution.Types.BuildInfo (BuildInfo (..))
import Distribution.Types.Library (Library (..))
import Distribution.Types.LocalBuildInfo (LocalBuildInfo (..))
import Distribution.Types.PackageDescription (PackageDescription (..))

import System.Directory (canonicalizePath, doesDirectoryExist, doesFileExist, findExecutable)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath
import System.Process (readProcessWithExitCode)

libJvmName :: FilePath
libJvmName = case buildOS of
  Windows -> "libjvm.dll"
  OSX -> "libjvm.dylib"
  _ -> "libjvm.so"

parents :: FilePath -> [FilePath]
parents p =
  case takeDirectory p of
    parent
      | parent == p -> []
      | otherwise -> parent : parents parent

-- from https://github.com/mesonbuild/meson/blob/0c3e84bbaf39a4b7f6dfd48faaee7adf61287b36/mesonbuild/dependencies/dev.py#L568-L584
getJavaHome :: IO (Maybe FilePath)
getJavaHome = do
  lookupEnv "JAVA_HOME" >>= \case
    Just javaHome -> pure (Just javaHome)
    Nothing -> do
      findExecutable "javac" >>= \case
        Nothing -> pure Nothing
        Just javacLoc -> case buildOS of
          OSX
            | any (`elem` parents javacLoc) ["/usr/bin", "/System/Library/Frameworks/JavaVM.framework/Versions"] ->
                readProcessWithExitCode "/usr/libexec/java_home" ["--failfast"] "" >>= \case
                  (ExitFailure _, _, _) -> pure Nothing
                  (ExitSuccess, stdout, _) -> Just <$> canonicalizePath (strip stdout)
          _ -> do
            actualJavacLoc <- canonicalizePath javacLoc
            let jdkBinDir = takeDirectory actualJavacLoc
                jdkDir = takeDirectory jdkBinDir
            pure . Just $ jdkDir
  where
    strip = dropWhile isSpace . dropWhileEnd isSpace

platformIncludeDir :: String
platformIncludeDir =
  case buildOS of
    Linux -> "linux"
    Windows -> "win32"
    OSX -> "darwin"
    Solaris -> "solaris"
    FreeBSD -> "freebsd"
    NetBSD -> "netbsd"
    OpenBSD -> "openbsd"
    DragonFly -> "dragonfly"
    os -> error $ "Unsupported OS: " <> show os <> ". Cannot find libjvm"

archLibDir :: String
archLibDir =
  case buildArch of
    I386 -> "x86"
    X86_64 -> "amd64"
    PPC -> "ppc"
    PPC64 -> "ppc64"
    Sparc -> "sparc"
    Arm -> "arm"
    AArch64 -> "aarch64"
    Mips -> "mips"
    IA64 -> "ia64"
    arch -> error $ "Unsupported architecture: " <> show arch <> ". Cannot find libjvm"

getJvmConf :: FilePath -> IO ([FilePath], [FilePath])
getJvmConf javaHome = do
  let includes = [javaHome </> "include", javaHome </> "include" </> platformIncludeDir]
  javaHomeServer <-
    case buildOS of
      Windows -> pure $ javaHome </> "lib"
      _ ->
        -- JVM after 8 moved location of libjvm
        doesDirectoryExist (javaHome </> "jre" </> "lib") <&> \case
          True -> javaHome </> "jre" </> "lib" </> archLibDir </> "server"
          False -> javaHome </> "lib" </> "server"
  let expectedPath = javaHomeServer </> libJvmName
  doesFileExist expectedPath >>= \case
    True -> pure ()
    False -> error $ "libjvm doesn't exist at expected path " <> expectedPath <> ". Cannot continue"
  pure (includes, [javaHomeServer])

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { preConf = \args flags -> do
      mJavaHome <- getJavaHome
      unless (isJust javaHome) $
        error "Could not find a suitable JVM. Try setting JAVA_HOME"
      let javaHome = fromJust javaHome
      (jvmIncludeDirs, jvmLibDirs) <- getJvmConf javaHome
      -- a tiny digest so JDK *contents* influence the package hash
      digest <- take 8 . show . SHA256.hashlazy <$> BL.readFile (javaHome </> "release")
      let tag = "-DJDK_DIGEST=" ++ digest
      (hooked, pd) <- confHook simpleUserHooks (genericPackageDescription flags)
                        ((), emptyHookedBuildInfo)
      let bi = emptyBuildInfo
               { extraLibDirs     = jvmLibDirs
               , extraIncludeDirs = jvmIncludeDirs
               , ccOptions        = [tag]
               }
      pure (Just (pd { library = mapBuildInfo (++ bi) <$> library pd }), hooked)
  }
