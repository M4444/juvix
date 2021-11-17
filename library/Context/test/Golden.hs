module Golden where

import Juvix.Library
import Juvix.Library.Test.Golden

discoverGoldenTestsContext ::
  -- | the directory in which to recursively look for golden tests
  FilePath ->
  IO [TestTree]
discoverGoldenTestsContext =
  discoverGoldenTestPasses
    (\contextPass -> expectSuccess . toNoQuotesCompact (handleDiscoverFunction contextPass))
    discoverContext
  where
    handleDiscoverFunction contextPass filePath = do
      let directory = FP.dropFileName filePath
      deps <- liftIO (fmap (directory <>) <$> findFileDependencies filePath)
      print deps
      desugaredPath <- fullyDesugarPath (filePath : (deps <> library))
      handleContextPass desugaredPath contextPass

