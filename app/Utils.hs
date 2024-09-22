module Utils where

import Network.Mail.Mime (Address (Address))
import Data.Text (Text)
import Control.Monad.Reader (Reader, ReaderT (..), runReader, runReaderT)
import Control.Monad.Reader.Class (asks)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import System.Directory (doesDirectoryExist, doesPathExist)
import qualified Data.Text as T
import System.Info (os)
import qualified System.Process as P
import System.Exit (ExitCode(..))

data OS = Windows | Linux | MacOS | Other deriving (Show)

getOS :: OS
getOS = case os of
  "mingw32" -> Windows
  "darwin"  -> MacOS
  "linux"   -> Linux
  _         -> Other

-- | 创建一个发件人地址
--
-- @param text 发件人的邮箱地址
-- @return 发件人的 Address 对象
from :: Text -> Address
from = Address Nothing

-- | 创建一个收件人地址
--
-- @param text 收件人的邮箱地址
-- @return 收件人的 Address 对象
to :: Text -> Address
to = Address Nothing

-- | 将 Reader 转换为 ReaderT
--
-- @param reader Reader 对象
-- @return 转换后的 ReaderT 对象
liftReader :: (Monad m) => Reader r a -> ReaderT r m a
liftReader = asks . runReader

liftMaybe :: (Monad m) => Maybe a -> m (Maybe a)
liftMaybe = return

-- | 运行 ReaderT，处理 Either 类型的结果
--
-- @param either 包含环境或错误的 Either 对象
-- @param readerT 要运行的 ReaderT 对象
-- @return 包含结果或错误的 Either 对象
runReaderTE :: (Monad m) => ReaderT r m a -> Either e r -> m (Either e a)
runReaderTE readerT = mapM (runReaderT readerT)

notNull :: ByteString -> Bool
notNull = not . BL.null

data FileType = Directory | File | NotExist deriving (Eq, Show)

getFileType :: FilePath -> IO FileType
getFileType fp = doesPathExist fp >>= fromPathExist
  where fromPathExist False = pure NotExist
        fromPathExist True  = doesDirectoryExist fp
                          >>= \case True -> pure Directory
                                    False -> pure File

readProcessWithExitCode :: Text -> [Text] -> Text -> IO (ExitCode, Text, Text)
readProcessWithExitCode cmd args input = do
  (exitCode, stdout, stderr) <- P.readProcessWithExitCode (T.unpack cmd) (map T.unpack args) (T.unpack input)
  return (exitCode, T.pack stdout, T.pack stderr)

notf :: (a -> Bool) -> (a -> Bool)
notf f x = not (f x)
