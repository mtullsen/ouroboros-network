{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Ouroboros.Network.Protocol.TxSubmission2.Codec
  ( codecTxSubmission2
  , codecTxSubmission2Id
  , encodeTxSubmission2
  , decodeTxSubmission2
  , byteLimitsTxSubmission2
  , timeLimitsTxSubmission2
  ) where

import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadTime
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Singletons

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import           Data.ByteString.Lazy (ByteString)
import           Text.Printf

import           Network.TypedProtocol.Codec.CBOR

import           Ouroboros.Network.Driver.Limits
import           Ouroboros.Network.Protocol.Limits
import           Ouroboros.Network.Protocol.TxSubmission2.Type

-- | Byte Limits.
byteLimitsTxSubmission2 :: forall bytes txid tx.
                           (bytes -> Word)
                        -> ProtocolSizeLimits (TxSubmission2 txid tx) bytes
byteLimitsTxSubmission2 = ProtocolSizeLimits stateToLimit
  where
    stateToLimit :: forall (st :: TxSubmission2 txid tx).
                    SingPeerHasAgency st -> Word
    stateToLimit (SingClientHasAgency  SingInit)                   = smallByteLimit
    stateToLimit (SingClientHasAgency (SingTxIds SingBlocking))    = largeByteLimit
    stateToLimit (SingClientHasAgency (SingTxIds SingNonBlocking)) = largeByteLimit
    stateToLimit (SingClientHasAgency  SingTxs)                    = largeByteLimit
    stateToLimit (SingServerHasAgency  SingIdle)                   = smallByteLimit


-- | Time Limits.
--
-- `TokTxIds TokBlocking` No timeout
-- `TokTxIds TokNonBlocking` `shortWait` timeout
-- `TokTxs` `shortWait` timeout
-- `TokIdle` `shortWait` timeout
timeLimitsTxSubmission2 :: forall txid tx. ProtocolTimeLimits (TxSubmission2 txid tx)
timeLimitsTxSubmission2 = ProtocolTimeLimits stateToLimit
  where
    stateToLimit :: forall (st :: TxSubmission2 txid tx).
                    SingPeerHasAgency st -> Maybe DiffTime
    stateToLimit (SingClientHasAgency  SingInit)                   = waitForever
    stateToLimit (SingClientHasAgency (SingTxIds SingBlocking))    = waitForever
    stateToLimit (SingClientHasAgency (SingTxIds SingNonBlocking)) = shortWait
    stateToLimit (SingClientHasAgency  SingTxs)                    = shortWait
    stateToLimit (SingServerHasAgency  SingIdle)                   = waitForever


codecTxSubmission2
  :: forall txid tx m.
     MonadST m
  => (txid -> CBOR.Encoding)
  -> (forall s . CBOR.Decoder s txid)
  -> (tx -> CBOR.Encoding)
  -> (forall s . CBOR.Decoder s tx)
  -> Codec (TxSubmission2 txid tx) CBOR.DeserialiseFailure m ByteString
codecTxSubmission2 encodeTxId decodeTxId
                   encodeTx   decodeTx =
    mkCodecCborLazyBS
      (encodeTxSubmission2 encodeTxId encodeTx)
      decode
  where
    decode :: forall (st :: TxSubmission2 txid tx).
              SingI (PeerHasAgency st)
           => forall s. CBOR.Decoder s (SomeMessage st)
    decode = do
      len <- CBOR.decodeListLen
      key <- CBOR.decodeWord
      decodeTxSubmission2 decodeTxId decodeTx len key

encodeTxSubmission2
    :: forall txid tx.
       (txid -> CBOR.Encoding)
    -> (tx -> CBOR.Encoding)
    -> (forall (st :: TxSubmission2 txid tx) (st' :: TxSubmission2 txid tx).
               Message (TxSubmission2 txid tx) st st'
            -> CBOR.Encoding)
encodeTxSubmission2 encodeTxId encodeTx = encode
  where
    encode :: forall st st'.
              Message (TxSubmission2 txid tx) st st'
           -> CBOR.Encoding
    encode  MsgInit =
        CBOR.encodeListLen 1
     <> CBOR.encodeWord 6

    encode (MsgRequestTxIds blocking ackNo reqNo) =
        CBOR.encodeListLen 4
     <> CBOR.encodeWord 0
     <> CBOR.encodeBool (case blocking of
                           SingBlocking    -> True
                           SingNonBlocking -> False)
     <> CBOR.encodeWord16 ackNo
     <> CBOR.encodeWord16 reqNo

    encode (MsgReplyTxIds txids) =
        CBOR.encodeListLen 2
     <> CBOR.encodeWord 1
     <> CBOR.encodeListLenIndef
     <> foldr (\(txid, sz) r -> CBOR.encodeListLen 2
                             <> encodeTxId txid
                             <> CBOR.encodeWord32 sz
                             <> r)
              CBOR.encodeBreak
              txids'
      where
        txids' :: [(txid, TxSizeInBytes)]
        txids' = case txids of
                   BlockingReply    xs -> NonEmpty.toList xs
                   NonBlockingReply xs -> xs

    encode (MsgRequestTxs txids) =
        CBOR.encodeListLen 2
     <> CBOR.encodeWord 2
     <> CBOR.encodeListLenIndef
     <> foldr (\txid r -> encodeTxId txid <> r) CBOR.encodeBreak txids

    encode (MsgReplyTxs txs) =
        CBOR.encodeListLen 2
     <> CBOR.encodeWord 3
     <> CBOR.encodeListLenIndef
     <> foldr (\txid r -> encodeTx txid <> r) CBOR.encodeBreak txs

    encode MsgDone =
        CBOR.encodeListLen 1
     <> CBOR.encodeWord 4


decodeTxSubmission2
    :: forall txid tx.
       (forall s . CBOR.Decoder s txid)
    -> (forall s . CBOR.Decoder s tx)
    -> (forall (st :: TxSubmission2 txid tx) s.
               SingI (PeerHasAgency st)
            => Int
            -> Word
            -> CBOR.Decoder s (SomeMessage st))
decodeTxSubmission2 decodeTxId decodeTx = decode
  where
    decode :: forall s (st :: TxSubmission2 txid tx).
              SingI (PeerHasAgency st)
           => Int
           -> Word
           -> CBOR.Decoder s (SomeMessage st)
    decode len key = do
      -- todo: don't use `phaState`
      let stok :: SingPeerHasAgency st
          stok = sing
      case (stok, len, key) of
        (SingClientHasAgency SingInit, 1, 6) ->
          return (SomeMessage MsgInit)

        (SingServerHasAgency SingIdle, 4, 0) -> do
          blocking <- CBOR.decodeBool
          ackNo    <- CBOR.decodeWord16
          reqNo    <- CBOR.decodeWord16
          return $! case blocking of
            True  -> SomeMessage (MsgRequestTxIds SingBlocking    ackNo reqNo)
            False -> SomeMessage (MsgRequestTxIds SingNonBlocking ackNo reqNo)

        (SingClientHasAgency (SingTxIds b),  2, 1) -> do
          CBOR.decodeListLenIndef
          txids <- CBOR.decodeSequenceLenIndef
                     (flip (:)) [] reverse
                     (do CBOR.decodeListLenOf 2
                         txid <- decodeTxId
                         sz   <- CBOR.decodeWord32
                         return (txid, sz))
          case (b, txids) of
            (SingBlocking, t:ts) ->
              return $
                SomeMessage (MsgReplyTxIds (BlockingReply (t NonEmpty.:| ts)))

            (SingNonBlocking, ts) ->
              return $
                SomeMessage (MsgReplyTxIds (NonBlockingReply ts))

            (SingBlocking, []) ->
              fail "codecTxSubmission2: MsgReplyTxIds: empty list not permitted"


        (SingServerHasAgency SingIdle, 2, 2) -> do
          CBOR.decodeListLenIndef
          txids <- CBOR.decodeSequenceLenIndef (flip (:)) [] reverse decodeTxId
          return (SomeMessage (MsgRequestTxs txids))

        (SingClientHasAgency SingTxs, 2, 3) -> do
          CBOR.decodeListLenIndef
          txids <- CBOR.decodeSequenceLenIndef (flip (:)) [] reverse decodeTx
          return (SomeMessage (MsgReplyTxs txids))

        (SingClientHasAgency (SingTxIds SingBlocking), 1, 4) ->
          return (SomeMessage MsgDone)

        --
        -- failures per protocol state
        --

        (_, _, _) ->
            fail (printf "codecTxSubmission2 (%s) unexpected key (%d, %d)" (show stok) key len)

codecTxSubmission2Id
  :: forall txid tx m. Monad m
  => Codec (TxSubmission2 txid tx) CodecFailure m (AnyMessage (TxSubmission2 txid tx))
codecTxSubmission2Id = Codec encode decode
 where
  encode :: forall  st st'.
            SingI (PeerHasAgency st)
         => Message (TxSubmission2 txid tx) st st'
         -> AnyMessage (TxSubmission2 txid tx)
  encode = AnyMessage

  decode :: forall (st :: TxSubmission2 txid tx).
            SingI (PeerHasAgency st)
         => m (DecodeStep (AnyMessage (TxSubmission2 txid tx))
                          CodecFailure m (SomeMessage st))
  decode = return $ DecodePartial $ \bytes -> return $
    case (phaState sing :: Sing st, bytes) of
      (SingInit,      Just (AnyMessage msg@MsgInit))              -> DecodeDone (SomeMessage msg) Nothing
      (SingIdle,      Just (AnyMessage msg@(MsgRequestTxIds SingBlocking _ _))) ->
        DecodeDone (SomeMessage msg) Nothing
      (SingIdle,      Just (AnyMessage msg@(MsgRequestTxIds SingNonBlocking _ _))) ->
        DecodeDone (SomeMessage msg) Nothing
      (SingIdle,      Just (AnyMessage msg@(MsgRequestTxs {}))) ->
        DecodeDone (SomeMessage msg) Nothing
      (SingTxs,       Just (AnyMessage msg@(MsgReplyTxs {}))) ->
        DecodeDone (SomeMessage msg) Nothing
      (SingTxIds b, Just (AnyMessage msg)) -> case (b, msg) of
        (SingBlocking,    MsgReplyTxIds (BlockingReply {})) ->
          DecodeDone (SomeMessage msg) Nothing
        (SingNonBlocking, MsgReplyTxIds (NonBlockingReply {})) ->
          DecodeDone (SomeMessage msg) Nothing
        (SingBlocking,    MsgDone {}) ->
          DecodeDone (SomeMessage msg) Nothing
        (_, _) -> DecodeFail (CodecFailure "codecTxSubmissionId: no matching message")
      (_, _) -> DecodeFail (CodecFailure "codecTxSubmissionId: no matching message")
