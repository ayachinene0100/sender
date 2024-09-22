{-# OPTIONS_GHC -Wno-orphans #-}

module Smtp (
  SmtpConfig(..),
  SmtpIO,
  readConfig,
  sendFile,
  sendFiles,
  sendFileWithLog
) where

import GHC.Generics (Generic)

import Network.Socket (HostName, PortNumber)
import Network.Mail.SMTP.Auth (UserName, Password)
import Network.Mail.Mime ( Mail(..), addAttachmentBS, Address, plainPart )
import Network.Mail.SMTP (sendMailWithLogin', simpleMail)

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader ( ReaderT, Reader, ask )
import Control.Monad.List (forM_)
import Control.Monad.Reader.Class (asks)

import System.FilePath ( takeFileName, (</>) )

import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Data.Yaml
 ( FromJSON
 , parseJSON
 , ToJSON
 , toJSON
 , Parser
 , decodeFileEither
 )

import Data.Word (Word16)

import ContentType (otectStream)
import Utils (from, liftReader)

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.Yaml.Aeson (ParseException)

data SmtpConfig = SmtpConfig {
  smtpHost :: HostName,
  smtpPort :: PortNumber,
  smtpUser :: UserName,
  smtpPassword :: Password
} deriving (Show, Generic)

instance FromJSON PortNumber where
  parseJSON v = fromIntegral <$> (parseJSON v :: Parser Word16)

instance ToJSON PortNumber where
  toJSON = toJSON . (fromIntegral :: PortNumber -> Word16)

instance FromJSON SmtpConfig
instance ToJSON SmtpConfig

readConfig' :: FilePath -> FilePath -> IO (Either ParseException SmtpConfig)
readConfig' dir file = decodeFileEither (dir </> file)

readConfig :: FilePath -> IO (Either ParseException SmtpConfig)
readConfig = readConfig' "config"

type SmtpIO = ReaderT SmtpConfig IO

-- | 向邮件中添加文件附件
-- 
-- @param fileName 文件名
-- @param fileContent 文件内容
-- @param mail 原始邮件
-- @return 添加附件后的邮件
addFile :: Text -> ByteString -> Mail -> Mail
addFile = addAttachmentBS otectStream

-- | 创建带有附件的邮件
-- 
-- @param to 收件人地址
-- @param subject 邮件主题
-- @param filePath 附件文件路径
-- @param bs 文件内容
-- @return 创建的邮件
createMailWithAttachment :: Address -> Text -> FilePath -> ByteString -> Reader SmtpConfig Mail
createMailWithAttachment to subject filePath bs =
      asks smtpUser
  >>= mailFromUser
  >>= addFile'
  where mailFromUser user = pure $ simpleMail (from (T.pack user)) [to] [] [] subject [plainPart ""]
        filename          = T.pack . takeFileName
        addFile'     mail = pure $ addFile (filename filePath) bs mail


-- | 发送邮件
-- 
-- @param mail 要发送的邮件
sendMail :: Mail -> SmtpIO ()
sendMail mail = ask >>= sendWithConfig
  where sendWithConfig SmtpConfig {..} = liftIO $ sendMailWithLogin'
                                                  smtpHost
                                                  smtpPort
                                                  smtpUser
                                                  smtpPassword
                                                  mail

-- | 发送带有文件附件的邮件
-- 
-- @param to 收件人地址
-- @param subject 邮件主题
-- @param filePath 附件文件路径
sendFile :: Address -> Text -> FilePath -> SmtpIO ()
sendFile to subject filePath =
      liftIO (BL.readFile filePath)
  >>= liftReader . createMailWithAttachment to subject filePath
  >>= sendMail

-- | 发送带有多个文件附件的邮件
-- 
-- @param to 收件人地址
-- @param xs 邮件主题和附件文件路径列表
sendFiles :: Address -> [(Text, FilePath)] -> SmtpIO ()
sendFiles to xs =forM_ xs (uncurry $ sendFileWithLog to)

-- | 发送单个文件附件的邮件
-- 
-- @param to 收件人地址
-- @param subject 邮件主题
-- @param filePath 附件文件路径
sendFileWithLog :: Address -> Text -> FilePath -> SmtpIO ()
sendFileWithLog to subject filePath =
     liftIO (putStr $ "Sending " ++ filePath ++ "...")
  >> sendFile to subject filePath
  >> liftIO (putStrLn " Done.")


