{-# LANGUAGE BlockArguments #-}
module Main where

import Prelude hiding (zip)
import Zip (zip, isZip, FileType(..))
import System.FilePath ((<.>), takeBaseName, takeFileName, replaceExtension, takeExtension, (</>))
import Utils (getFileType, to, liftMaybe)
import Control.Monad.Reader (ReaderT(..))
import Split (DataUnit(MB), createChunk, splitFile, toBytes, Chunk)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad ( forM_, when )
import System.FilePath.Glob (glob)
import System.Directory (getFileSize, createDirectoryIfMissing)
import Smtp (SmtpConfig, sendFiles, readConfig)

sendCompress :: SmtpConfig -> Text -> FilePath -> IO ()
sendCompress cfg receiver fp = do
    fileType <- getFileType fp
    if fileType == NotExist
    then putStrLn $ fp ++ " does not exist"
    else do
        zipFile <- prepareZipFile fp
        chunk <- liftMaybe $ createChunk 20 MB
        forM_ chunk \c -> do
            needSplit <- shouldSplit zipFile c
            if needSplit
               then sendSplitFiles cfg receiver zipFile c
               else sendWholeFile cfg receiver zipFile

prepareZipFile :: FilePath -> IO FilePath
prepareZipFile fp = do
    let outputDir = "output"
    createDirectoryIfMissing True outputDir
    let absPathInOutputDir = (outputDir </>)
    
    let shouldZip = not $ isZip fp
    let zipFile = if shouldZip then absPathInOutputDir $ takeBaseName fp <.> "zip" else fp
    when shouldZip (zip fp zipFile)
    return zipFile

sendSplitFiles :: SmtpConfig -> Text -> FilePath -> Chunk -> IO ()
sendSplitFiles cfg receiver zipFile chunk = do
    splitFile chunk zipFile "output"
    splits <- findSplits zipFile
    runReaderT (sendFiles (to receiver) (nameFps splits)) cfg

sendWholeFile :: SmtpConfig -> Text -> FilePath -> IO ()
sendWholeFile cfg receiver zipFile = runReaderT (sendFiles (to receiver) [name zipFile]) cfg

shouldSplit :: FilePath -> Chunk -> IO Bool
shouldSplit filePath chunk = getFileSize filePath >>= check
  where check size = pure $ size > fromIntegral (toBytes chunk)

name :: FilePath -> (Text, FilePath)
name fp = (T.pack . takeFileName $ fp, fp)

nameFps :: [FilePath] -> [(Text, FilePath)]
nameFps = map name

findSplits :: FilePath -> IO [FilePath]
findSplits fp = fmap 
                  (filter (\x -> takeExtension x /= ".zip")) 
                  (glob $ replaceExtension fp "z*")

main :: IO ()
main = do
   smtpCfg <- readConfig "myConfig.yaml"
   forM_ smtpCfg \cfg -> do
      sendCompress cfg "ayachineneai@gmail.com" "config.zip"
