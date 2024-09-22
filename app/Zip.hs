module Zip (
  ZipTool(..),
  FileType(..),
  isZip,
  zipCmd',
  zipCmd,
  zip
) where

import Prelude hiding (zip)
import Data.Text (Text)
import qualified Data.Text as T
import Utils (FileType(..), OS (Windows), getFileType, readProcessWithExitCode, getOS, liftMaybe)
import System.Exit (ExitCode(..))
import Data.List (isSuffixOf)

data ZipTool = SevenZip | ZIP

-- | 生成压缩目录的命令
--
-- @param tool 压缩工具
-- @param dir 要压缩的目录路径
-- @param zipFile 生成的压缩文件路径
-- @return 包含命令和参数的元组
zipDirCmd :: ZipTool -> FilePath -> FilePath -> (Text, [Text])
zipDirCmd SevenZip dir zipFile = ("7z",  ["a", "-tzip", T.pack zipFile, T.pack dir <> "/*"])
zipDirCmd ZIP      dir zipFile = ("zip", ["-r", T.pack zipFile, T.pack dir])

-- | 生成压缩文件的命令
--
-- @param tool 压缩工具
-- @param file 要压缩的文件路径
-- @param zipFile 生成的压缩文件路径
-- @return 包含命令和参数的元组
zipFileCmd :: ZipTool -> FilePath -> FilePath -> (Text, [Text])
zipFileCmd SevenZip file zipFile = ("7z",  ["a", "-tzip", T.pack zipFile, T.pack file])
zipFileCmd ZIP      file zipFile = ("zip", ["-r", T.pack zipFile, T.pack file])

-- | 检查文件是否为zip文件
--
-- @param fp 文件路径
-- @return 如果是zip文件则返回True，否则返回False
isZip :: FilePath -> Bool
isZip fp = ".zip" `isSuffixOf` fp

-- | 根据文件类型生成压缩命令
--
-- @param tool 压缩工具
-- @param fp 要压缩的文件或目录路径
-- @param zipFile 生成的压缩文件路径
-- @return 包含命令和参数的Maybe元组
zipCmd' :: ZipTool -> FilePath -> FilePath -> IO (Maybe (Text, [Text]))
zipCmd' tool fp zipFile = getFileType fp >>= liftMaybe . cmdFromType
  where cmdFromType fileType = zipF fileType >>= \f -> pure $ f tool fp zipFile
        zipF fileType = case fileType of
          Directory -> Just zipDirCmd
          File -> Just zipFileCmd
          NotExist -> Nothing

-- | 根据操作系统选择压缩工具并生成压缩命令
--
-- @param fp 要压缩的文件或目录路径
-- @param zipFile 生成的压缩文件路径
-- @return 包含命令和参数的Maybe元组
zipCmd :: FilePath -> FilePath -> IO (Maybe (Text, [Text]))
zipCmd = zipCmd' zipTool
  where zipTool = case getOS of
          Windows -> SevenZip
          _       -> ZIP

-- | 执行压缩操作
--
-- @param fp 要压缩的文件或目录路径
-- @param zipFile 生成的压缩文件路径
-- @return IO ()
zip :: FilePath -> FilePath -> IO ()
zip fp zipFile = zipCmd fp zipFile >>= zipWithCmd
  where zipWithCmd = \case
              Just (cmd, args) -> do
                (exitCode, _, stderr) <- readProcessWithExitCode cmd args ""
                case exitCode of
                  ExitSuccess -> pure ()
                  ExitFailure _ -> error $ "Failed to zip " ++ fp ++ ": " ++ T.unpack stderr
              Nothing -> putStrLn $ show fp ++ "does not exist"