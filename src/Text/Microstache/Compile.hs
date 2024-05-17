-- |
-- Module      :  Text.Microstache.Compile
-- Copyright   :  © 2016–2017 Stack Builders
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Mustache 'Template' creation from file or a 'Text' value. You don't
-- usually need to import the module, because "Text.Microstache" re-exports
-- everything you may need, import that module instead.

module Text.Microstache.Compile
  ( compileMustacheDir
  , getMustacheFilesInDir
  , compileMustacheFile
  , compileMustacheText )
where

import Control.Exception (throwIO)
import Control.Monad     (filterM, foldM)
import Data.Text.Lazy    (Text)
import System.Directory
       (doesFileExist, getCurrentDirectory, getDirectoryContents)
import Text.Parsec       (ParseError)

import qualified Data.Map          as Map
import qualified Data.Text         as T
import qualified Data.Text.Lazy.IO as LT
import qualified System.FilePath   as F

import Text.Microstache.Parser
import Text.Microstache.Type

-- | Compile all templates in specified directory and select one. Template
-- files should have extension @mustache@, (e.g. @foo.mustache@) to be
-- recognized. This function /does not/ scan the directory recursively.
--
-- The action can throw the same exceptions as 'getDirectoryContents', and
-- 'T.readFile'.

compileMustacheDir
  :: PName             -- ^ Which template to select after compiling
  -> FilePath          -- ^ Directory with templates
  -> IO Template       -- ^ The resulting template
compileMustacheDir pname path =
  getMustacheFilesInDir path >>=
  fmap selectKey . foldM f (Template undefined Map.empty)
  where
    selectKey t = t { templateActual = pname }
    f (Template _ old) fp = do
      Template _ new <- compileMustacheFile fp
      return (Template undefined (Map.union new old))

-- | Return a list of templates found in given directory. The returned paths
-- are absolute.

getMustacheFilesInDir
  :: FilePath          -- ^ Directory with templates
  -> IO [FilePath]
getMustacheFilesInDir path =
  getDirectoryContents path >>=
  filterM isMustacheFile . fmap (F.combine path) >>=
  mapM makeAbsolute'

-- | Compile single Mustache template and select it.
--
-- The action can throw the same exceptions as 'T.readFile'.

compileMustacheFile
  :: FilePath          -- ^ Location of the file
  -> IO Template
compileMustacheFile path =
    LT.readFile path >>= withException . compile
  where
    pname = pathToPName path
    compile = fmap (Template pname . Map.singleton pname) . parseMustache path

-- | Compile Mustache template from a lazy 'Text' value. The cache will
-- contain only this template named according to given 'PName'.

compileMustacheText
  :: PName             -- ^ How to name the template?
  -> Text              -- ^ The template to compile
  -> Either ParseError Template -- ^ The result
compileMustacheText pname txt =
  Template pname . Map.singleton pname <$> parseMustache "" txt

----------------------------------------------------------------------------
-- Helpers

-- | Check if given 'FilePath' points to a mustache file.

isMustacheFile :: FilePath -> IO Bool
isMustacheFile path = do
  exists <- doesFileExist path
  let rightExtension = F.takeExtension path == ".mustache"
  return (exists && rightExtension)

-- | Build a 'PName' from given 'FilePath'.

pathToPName :: FilePath -> PName
pathToPName = PName . T.pack . F.takeBaseName

-- | Throw 'MustacheException' if argument is 'Left' or return the result
-- inside 'Right'.

withException
  :: Either ParseError Template -- ^ Value to process
  -> IO Template        -- ^ The result
withException = either (throwIO . MustacheParserException) return

makeAbsolute' :: FilePath -> IO FilePath
makeAbsolute' path0 =
    fmap (matchTrailingSeparator path0 . F.normalise) (prependCurrentDirectory path0)
  where
    prependCurrentDirectory :: FilePath -> IO FilePath
    prependCurrentDirectory path =
      if F.isRelative path -- avoid the call to `getCurrentDirectory` if we can
      then (F.</> path) <$> getCurrentDirectory
      else return path

    matchTrailingSeparator :: FilePath -> FilePath -> FilePath
    matchTrailingSeparator path
      | F.hasTrailingPathSeparator path = F.addTrailingPathSeparator
      | otherwise                       = F.dropTrailingPathSeparator
