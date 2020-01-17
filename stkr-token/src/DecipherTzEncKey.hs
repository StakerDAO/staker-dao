{-# LANGUAGE NoRebindableSyntax #-}

module DecipherTzEncKey (DecipherError(..), decipherTzEncKey, ScrubbedView) where

import Prelude

import Data.Text (Text)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import qualified Codec.Binary.UTF8.String as UTF8
import Foreign.Storable (peek, poke)
import Foreign.Marshal.Array (pokeArray)
import Foreign.Ptr (castPtr)
import Foreign.C.String (CString)
import Foreign.C (CInt(..))
import Data.Word (Word64)

import qualified Data.ByteArray as B

import Crypto.KDF.PBKDF2 (Parameters(..), fastPBKDF2_SHA512)

import Tezos.Crypto (B58CheckWithPrefixError, decodeBase58CheckWithPrefix)

foreign import ccall unsafe crypto_secretbox_open :: {- mut -} CString -> CString -> Word64 -> CString -> CString -> IO CInt

data DecipherError
  = DeciferB58Error B58CheckWithPrefixError
  | DeciferWrongEncryptionKeyLength
  | DeciferWrongCipher
  deriving stock (Show)

type ScrubbedView = B.View B.ScrubbedBytes

sodiumSecretboxOpen :: BS.ByteString -> BS.ByteString -> BS.ByteString -> IO (Either DecipherError ScrubbedView)
sodiumSecretboxOpen c nonce k
  | BS.length k /= 32 = pure $ Left DeciferWrongEncryptionKeyLength
  | otherwise = do
      opened <- B.alloc plen ini
      -- reuse the first bytes for ret code (see below)
      r <- B.withByteArray opened (peek @CInt)
      pure $
        if r == 0
          then Right $ B.view opened 32 (plen - 32)
          else Left DeciferWrongCipher
  where
    ini mptr = do
             BSU.unsafeUseAsCString padded $ \pptr ->
               BSU.unsafeUseAsCString nonce $ \nptr ->
                 BSU.unsafeUseAsCString k $ \kptr -> do
                   r <- crypto_secretbox_open mptr pptr (fromIntegral plen) nptr kptr
                   -- Inelegant, reuse first bytes to put retcode into.
                   -- ALTERNATIVES:
                   --   1. use exceptions (exceptions are not particularly good when used for control flow)
                   --   2. allocate separate buffer for the ret value (overkill)
                   poke (castPtr mptr) r
    padded = BS.replicate 16 0 `BS.append` c
    plen = BS.length padded

-- We need scrubbed password here, otherwise `fastPBKDF2_SHA512` simply pokes
--   the string into buffer.
decryptKeyBytes :: BS.ByteString -> B.ScrubbedBytes -> IO (Either DecipherError ScrubbedView)
decryptKeyBytes keyBytes password = sodiumSecretboxOpen key nonce encryptionKey
  where
    nonce = BS.replicate 24 0
    (salt, key) = BS.splitAt 8 keyBytes
    encryptionKey = fastPBKDF2_SHA512 (Parameters 32768 32) password salt

decodeKeyBytes :: Text -> Either B58CheckWithPrefixError BS.ByteString
decodeKeyBytes = decodeBase58CheckWithPrefix ed25589_enc_prefix
  where
    ed25589_enc_prefix = BS.pack [7, 90, 60, 179, 41]

-- We have the password as a String (list of char), pretend it's safe if not
--   poked into a non-scrubbable buffer.
decipherTzEncKey :: Text -> String -> IO (Either DecipherError ScrubbedView)
decipherTzEncKey t password =
  case decodeKeyBytes t of
    Right kb -> B.alloc (length passutf8) (flip pokeArray passutf8) >>= decryptKeyBytes kb
    Left err -> pure (Left $ DeciferB58Error err)
  where
    passutf8 = UTF8.encode password
