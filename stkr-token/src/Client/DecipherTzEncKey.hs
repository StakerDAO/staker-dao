{-# LANGUAGE NoRebindableSyntax #-}

module Client.DecipherTzEncKey (DecipherError(..), decipherTzEncKey) where

import Prelude

import Data.Text (Text)
import qualified Data.Text.Encoding as TE (encodeUtf8)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (plusPtr)
import Foreign.C.String (CString)
import Data.Word (Word64)

import Crypto.KDF.PBKDF2 (Parameters(..), fastPBKDF2_SHA512)

import Tezos.Crypto (B58CheckWithPrefixError, decodeBase58CheckWithPrefix)

foreign import ccall unsafe crypto_secretbox_open :: {- mut -} CString -> CString -> Word64 -> CString -> CString -> IO Int

data DecipherError
  = DeciferB58Error B58CheckWithPrefixError
  | DeciferWrongEncryptionKeyLength
  | DeciferWrongCipher
  deriving stock (Show)

sodiumSecretboxOpen :: BS.ByteString -> BS.ByteString -> BS.ByteString -> IO (Either DecipherError BS.ByteString)
sodiumSecretboxOpen c nonce k
  | BS.length k /= 32 = err DeciferWrongEncryptionKeyLength
  | otherwise = allocaBytes plen $ \mptr ->
                  BSU.unsafeUseAsCString padded $ \pptr ->
                  BSU.unsafeUseAsCString nonce $ \nptr ->
                  BSU.unsafeUseAsCString k $ \kptr -> do
                    r <- crypto_secretbox_open mptr pptr (fromIntegral plen) nptr kptr
                    if r == 0
                      then Right <$> BSU.unsafePackCStringLen (plusPtr mptr 32, plen - 32)
                      else err DeciferWrongCipher
  where
    padded = BS.replicate 16 0 `BS.append` c
    plen = BS.length padded
    err = pure . Left

decryptKeyBytes :: BS.ByteString -> Text -> IO (Either DecipherError BS.ByteString)
decryptKeyBytes keyBytes password = sodiumSecretboxOpen key nonce encryptionKey
  where
    nonce = BS.replicate 24 0
    (salt, key) = BS.splitAt 8 keyBytes
    encryptionKey = fastPBKDF2_SHA512 (Parameters 32768 32) (TE.encodeUtf8 password) salt

decodeKeyBytes :: Text -> Either B58CheckWithPrefixError BS.ByteString
decodeKeyBytes = decodeBase58CheckWithPrefix ed25589_enc_prefix
  where
    ed25589_enc_prefix = BS.pack [7, 90, 60, 179, 41]

decipherTzEncKey :: Text -> Text -> IO (Either DecipherError BS.ByteString)
decipherTzEncKey t password =
    case decodeKeyBytes t of
      Right kb -> decryptKeyBytes kb password
      Left err -> pure (Left $ DeciferB58Error err)
