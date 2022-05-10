{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Ouroboros.Network.Protocol.Handshake.Codec
  ( codecHandshake
  , byteLimitsHandshake
  , timeLimitsHandshake
  , noTimeLimitsHandshake
  , encodeRefuseReason
  , decodeRefuseReason
    -- ** Version data codec
  , VersionDataCodec (..)
  , cborTermVersionDataCodec
  ) where

import           Control.Monad (replicateM, unless)
import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadTime
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import           Data.Either (partitionEithers)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (mapMaybe)
import           Data.Singletons
import           Data.Text (Text)
import           Text.Printf

import           Network.TypedProtocol.Codec.CBOR
import           Network.TypedProtocol.Core

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Term as CBOR

import           Ouroboros.Network.CodecCBORTerm
import           Ouroboros.Network.Driver.Limits

import           Ouroboros.Network.Protocol.Handshake.Type
import           Ouroboros.Network.Protocol.Limits

-- | Codec for version data ('vData' in code) exchanged by the handshake
-- protocol.
--
-- Note: 'extra' type param is instantiated to 'DictVersion'; 'agreedOptions'
-- is instantiated to 'NodeToNodeVersionData' in "Ouroboros.Network.NodeToNode"
-- or to '()' in "Ouroboros.Network.NodeToClient".
--
data VersionDataCodec bytes vNumber vData = VersionDataCodec {
    encodeData :: vNumber -> vData -> bytes,
    -- ^ encoder of 'vData' which has access to 'extra vData' which can bring
    -- extra instances into the scope (by means of pattern matching on a GADT).
    decodeData :: vNumber -> bytes -> Either Text vData
    -- ^ decoder of 'vData'.
  }

-- TODO: remove this from top level API, this is the only way we encode or
-- decode version data.
cborTermVersionDataCodec :: (vNumber -> CodecCBORTerm Text vData)
                         -> VersionDataCodec CBOR.Term vNumber vData
cborTermVersionDataCodec codec = VersionDataCodec {
      encodeData = encodeTerm . codec,
      decodeData = decodeTerm . codec
    }

-- |
-- We assume that a TCP segment size of 1440 bytes with initial window of size
-- 4.  This sets upper limit of 5760 bytes on each message of handshake
-- protocol.
--
maxTransmissionUnit :: Word
maxTransmissionUnit = 4 * 1440

-- | Byte limits
byteLimitsHandshake :: forall vNumber. ProtocolSizeLimits (Handshake vNumber CBOR.Term) ByteString
byteLimitsHandshake = ProtocolSizeLimits stateToLimit (fromIntegral . BL.length)
  where
    stateToLimit :: forall (st  :: Handshake vNumber CBOR.Term).
                    SingPeerHasAgency st -> Word
    stateToLimit (SingClientHasAgency SingPropose) =
      maxTransmissionUnit
    stateToLimit (SingServerHasAgency SingConfirm) =
      maxTransmissionUnit

-- | Time limits.
--
timeLimitsHandshake :: forall vNumber. ProtocolTimeLimits (Handshake vNumber CBOR.Term)
timeLimitsHandshake = ProtocolTimeLimits stateToLimit
  where
    stateToLimit :: forall (st  :: Handshake vNumber CBOR.Term).
                    SingPeerHasAgency st -> Maybe DiffTime
    stateToLimit (SingClientHasAgency SingPropose) = shortWait
    stateToLimit (SingServerHasAgency SingConfirm) = shortWait


noTimeLimitsHandshake :: forall vNumber. ProtocolTimeLimits (Handshake vNumber CBOR.Term)
noTimeLimitsHandshake = ProtocolTimeLimits stateToLimit
  where
    stateToLimit :: forall (st  :: Handshake vNumber CBOR.Term).
                    SingPeerHasAgency st -> Maybe DiffTime
    stateToLimit (SingClientHasAgency SingPropose) = Nothing
    stateToLimit (SingServerHasAgency SingConfirm) = Nothing


-- |
-- @'Handshake'@ codec.  The @'MsgProposeVersions'@ encodes proposed map in
-- ascending order and it expects to receive them in this order.  This allows
-- to construct the map in linear time.  There is also another limiting factor
-- to the number of versions on can present: the whole message must fit into
-- a single TCP segment.
--
codecHandshake
  :: forall vNumber m failure.
     ( MonadST m
     , Ord vNumber
     , Show failure
     )
  => CodecCBORTerm (failure, Maybe Int) vNumber
  -> Codec (Handshake vNumber CBOR.Term) CBOR.DeserialiseFailure m ByteString
codecHandshake versionNumberCodec = mkCodecCborLazyBS encodeMsg decodeMsg
    where
      encodeMsg
        :: forall st st'.
           Message (Handshake vNumber CBOR.Term) st st'
        -> CBOR.Encoding

      encodeMsg (MsgProposeVersions vs) =
           CBOR.encodeListLen 2
        <> CBOR.encodeWord 0
        <> encodeVersions versionNumberCodec vs

      -- Although `MsgReplyVersions` shall not be sent, for testing purposes it
      -- is useful to have an encoder for it.
      encodeMsg (MsgReplyVersions vs) =
           CBOR.encodeListLen 2
        <> CBOR.encodeWord 0
        <> encodeVersions versionNumberCodec vs

      encodeMsg (MsgAcceptVersion vNumber vParams) =
           CBOR.encodeListLen 3
        <> CBOR.encodeWord 1
        <> CBOR.encodeTerm (encodeTerm versionNumberCodec vNumber)
        <> CBOR.encodeTerm vParams

      encodeMsg (MsgRefuse vReason) =
           CBOR.encodeListLen 2
        <> CBOR.encodeWord 2
        <> encodeRefuseReason versionNumberCodec vReason

      decodeMsg :: forall s (st :: Handshake vNumber CBOR.Term).
                   SingI (PeerHasAgency st)
                => CBOR.Decoder s (SomeMessage st)
      decodeMsg = do
        len <- CBOR.decodeListLen
        key <- CBOR.decodeWord
        case (sing @(PeerHasAgency st), key, len) of
          (SingClientHasAgency SingPropose, 0, 2) -> do
            l  <- CBOR.decodeMapLen
            vMap <- decodeVersions versionNumberCodec l
            pure $ SomeMessage $ MsgProposeVersions vMap
          (SingServerHasAgency SingConfirm, 0, 2) -> do
            l  <- CBOR.decodeMapLen
            vMap <- decodeVersions versionNumberCodec l
            pure $ SomeMessage $ MsgReplyVersions vMap
          (SingServerHasAgency SingConfirm, 1, 3) -> do
            v <- decodeTerm versionNumberCodec <$> CBOR.decodeTerm
            case v of
              -- at this stage we can throw exception when decoding
              -- version number: 'MsgAcceptVersion' must send us back
              -- version which we know how to decode
              Left e -> fail ("codecHandshake.MsgAcceptVersion: not recognized version: " ++ show e)
              Right vNumber ->
                SomeMessage . MsgAcceptVersion vNumber <$> CBOR.decodeTerm
          (SingServerHasAgency SingConfirm, 2, 2) ->
            SomeMessage . MsgRefuse <$> decodeRefuseReason versionNumberCodec

          (SingClientHasAgency stok@SingPropose, _, _) ->
            fail $ printf "codecHandshake (%s) unexpected key (%d, %d)" (show stok) key len
          (SingServerHasAgency stok@SingConfirm, _, _) ->
            fail $ printf "codecHandshake (%s) unexpected key (%d, %d)" (show stok) key len


-- | Encode version map preserving the ascending order of keys.
--
encodeVersions :: CodecCBORTerm (failure, Maybe Int) vNumber
               -> Map vNumber CBOR.Term
               -> CBOR.Encoding
encodeVersions versionNumberCodec vs =
       CBOR.encodeMapLen (fromIntegral (Map.size vs))
    <> Map.foldMapWithKey
        (\vNumber vParams ->
            CBOR.encodeTerm (encodeTerm versionNumberCodec vNumber)
         <> CBOR.encodeTerm vParams
        )
        vs


-- | decode a map checking the assumption that
--
-- * keys are different
-- * keys are encoded in ascending order
--
-- fail when one of these assumptions is not met.
--
decodeVersions :: forall vNumber failure s.
                  Ord vNumber
               => CodecCBORTerm (failure, Maybe Int) vNumber
               -> Int
               -> CBOR.Decoder s (Map vNumber CBOR.Term)
decodeVersions versionNumberCodec size = go size Nothing []
  where
    go :: Int
       -> Maybe vNumber
       -> [(vNumber, CBOR.Term)]
       -> CBOR.Decoder s (Map vNumber CBOR.Term)
    go 0  _     !vs = return $ Map.fromDistinctAscList $ reverse vs
    go !l !prev !vs = do
      vNumberTerm <- CBOR.decodeTerm
      vParams <- CBOR.decodeTerm
      case decodeTerm versionNumberCodec vNumberTerm of
        -- error when decoding un-recognized version; skip the version
        -- TODO: include error in the dictionary
        Left _        -> go (pred l) prev vs

        Right vNumber -> do
          let next = Just vNumber
          unless (next > prev)
            $ fail "codecHandshake.Propose: unordered version"
          go (pred l) next ((vNumber, vParams) : vs)


encodeRefuseReason :: CodecCBORTerm fail vNumber
                   -> RefuseReason vNumber
                   -> CBOR.Encoding
encodeRefuseReason versionNumberCodec (VersionMismatch vs _) =
         CBOR.encodeListLen 2
      <> CBOR.encodeWord 0
      <> CBOR.encodeListLen (fromIntegral $ length vs)
      <> foldMap (CBOR.encodeTerm . encodeTerm versionNumberCodec) vs
encodeRefuseReason versionNumberCodec (HandshakeDecodeError vNumber vError) =
         CBOR.encodeListLen 3
      <> CBOR.encodeWord 1
      <> CBOR.encodeTerm (encodeTerm versionNumberCodec vNumber)
      <> CBOR.encodeString vError
encodeRefuseReason versionNumberCodec (Refused vNumber vReason) =
         CBOR.encodeListLen 3
      <> CBOR.encodeWord 2
      <> CBOR.encodeTerm (encodeTerm versionNumberCodec vNumber)
      <> CBOR.encodeString vReason


decodeRefuseReason :: Show failure
                   => CodecCBORTerm (failure, Maybe Int) vNumber
                   -> CBOR.Decoder s (RefuseReason vNumber)
decodeRefuseReason versionNumberCodec = do
    _ <- CBOR.decodeListLen
    tag <- CBOR.decodeWord
    case tag of
      0 -> do
        len <- CBOR.decodeListLen
        rs <- replicateM len
                (decodeTerm versionNumberCodec <$> CBOR.decodeTerm)
        case partitionEithers rs of
          (errs, vNumbers) ->
            pure $ VersionMismatch vNumbers (mapMaybe snd errs)
      1 -> do
        v <- decodeTerm versionNumberCodec <$> CBOR.decodeTerm
        case v of
          Left e        -> fail $ "decode HandshakeDecodeError: unknow version: " ++ show e
          Right vNumber -> HandshakeDecodeError vNumber <$> CBOR.decodeString
      2 -> do
        v <- decodeTerm versionNumberCodec <$> CBOR.decodeTerm
        case v of
          Left e        -> fail $ "decode Refused: unknonwn version: " ++ show e
          Right vNumber -> Refused vNumber <$> CBOR.decodeString
      _ -> fail $ "decode RefuseReason: unknown tag " ++ show tag
