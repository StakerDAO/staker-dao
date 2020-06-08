module Lorentz.CryptoInterop
  ( PublicKey(..)
  , SecretKey
  , Signature(..)
  , KeyHash
  , sign
  , hashKey
  , detSecretKey
  , parseSecretKey
  , formatSecretKey
  , formatKeyHash
  , parsePublicKey
  , toPublic
  , blake2b
  , parseKeyHash
  , parseSignature
  , encodeBase58Check
  ) where

import Prelude

import Data.ByteString (ByteString)
import Tezos.Crypto
  (KeyHash, PublicKey(..), Signature(..), blake2b, encodeBase58Check,
  formatKeyHash, hashKey, parseKeyHash, parsePublicKey, parseSignature)
import Tezos.Crypto.Ed25519
  (SecretKey, detSecretKey, formatSecretKey, parseSecretKey)
import qualified Tezos.Crypto.Ed25519 as Ed25519

sign :: SecretKey -> ByteString -> Signature
sign sk bs = SignatureEd25519 $ Ed25519.sign sk bs

toPublic :: SecretKey -> PublicKey
toPublic = PublicKeyEd25519 . Ed25519.toPublic
