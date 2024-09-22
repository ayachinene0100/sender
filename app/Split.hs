module Split (
  Chunk,
  DataUnit(..),
  createChunk,
  split,
  defaultNamer,
  splitFile,
  toBytes
) where

import Data.ByteString.Lazy (ByteString)
import Data.List (unfoldr)
import Data.Int (Int64)
import System.FilePath (takeBaseName)
import System.FilePath.Windows ( (<.>), (</>) )
import System.Directory (createDirectoryIfMissing)
import Control.Monad (guard)
import Utils (notNull)

import qualified Data.ByteString.Lazy as BL


data DataUnit = B | KB | MB | GB | TB
  deriving (Show, Eq, Enum)

data Chunk = Chunk {
  chunkSize :: Int,
  chunkUnit :: DataUnit
}

-- | 创建一个 Chunk
-- 
-- @param size: 块大小
-- @param unit: 数据单位
-- @return: 如果 size 非负，返回 Just Chunk，否则返回 Nothing
createChunk :: Int -> DataUnit -> Maybe Chunk
createChunk size unit = guard (size >= 0) >> Just (Chunk size unit)

-- | 将 Chunk 转换为字节数
-- 
-- @param chunk: 要转换的 Chunk
-- @return: 字节数
toBytes :: Chunk -> Int
toBytes Chunk {..} = chunkSize * 1024 ^ fromEnum chunkUnit

toBytes64 :: Chunk -> Int64
toBytes64 = fromIntegral . toBytes

splitChunk :: Chunk -> ByteString -> (ByteString, ByteString)
splitChunk chunk = BL.splitAt (toBytes64 chunk)

-- | 将 ByteString 分割成多个指定大小的 ByteString
-- 
-- @param chunk: 指定分割大小的 Chunk
-- @param bs: 要分割的 ByteString
-- @return: 分割后的 ByteString 列表
split :: Chunk -> ByteString -> [ByteString]
split chunk = unfoldr split'
  where split' bs = guard (notNull bs) >> Just (splitChunk chunk bs)

-- | 创建默认的文件命名函数
-- 
-- @param path: 原始文件路径
-- @return: 一个函数，接受一个整数参数，返回新的文件名
defaultNamer :: FilePath -> (Int -> FilePath)
defaultNamer path i = takeBaseName path <.> "z" ++ show i

-- | 将文件分割成多个小文件
-- 
-- @param namer: 文件命名函数
-- @param chunk: 指定分割大小的 Chunk
-- @param path: 要分割的文件路径
-- @param outputDir: 输出目录
-- @return: IO ()
splitFile' :: (Int -> FilePath) -> Chunk -> FilePath -> FilePath -> IO ()
splitFile' namer chunk path outputDir =
     createDirectoryIfMissing True outputDir
 >>  BL.readFile path
 >>= splitBs
 >>= index
 >>= mapM_ write
  where splitBs   bs = pure $ split chunk bs
        index     xs = pure $ zip [1..] xs
        write (i, c) = BL.writeFile (outputDir </> namer i) c

splitFile :: Chunk -> FilePath -> FilePath -> IO ()
splitFile chunk path = splitFile' (defaultNamer path) chunk path