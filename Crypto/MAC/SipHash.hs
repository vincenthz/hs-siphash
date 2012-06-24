-- |
-- Module      : Crypto.MAC.SipHash
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : good
--
-- provide the SipHash algorithm.
-- reference: <http://131002.net/siphash/siphash.pdf>
--
module Crypto.MAC.SipHash
    ( SipKey(..)
    , SipHash(..)
    , hash
    , hashWith
    ) where

import Data.Word
import Data.Bits
import Data.List (foldl')
import Data.Serialize.Get
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Control.Monad (foldM,replicateM)

-- | SigHash Key
data SipKey = SipKey {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64

-- | Siphash tag value
newtype SipHash = SipHash Word64
    deriving (Show,Eq)

data InternalState = InternalState {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64

-- | produce a siphash with a key and a bytestring.
hash :: SipKey -> ByteString -> SipHash
hash = hashWith 2 4

-- | same as 'hash', except also specifies the number of sipround iterations for compression and digest.
hashWith :: Int -> Int -> SipKey -> ByteString -> SipHash
hashWith c d key b = either error (finish d) $ runGet runHash b
    where len = B.length b
          (nb,last) = len `divMod` 8

          runHash = foldM (const . getBlock) (initSip key)[0..(nb-1)] >>= lastBlock
          getBlock st = process c st `fmap` getWord64le
          lastBlock st = do
              let lengthBlock = fromIntegral (len `mod` 256) `shiftL` 56
              z <- (fst . foldl shiftAndAdd (0,0)) `fmap` replicateM last getWord8
              return $ process c st (lengthBlock .|. z)

          shiftAndAdd :: (Word64,Int) -> Word8 -> (Word64,Int)
          shiftAndAdd (acc,pos) v = (acc .|. ((fromIntegral v) `shiftL` pos), pos+8)

initSip (SipKey k0 k1) = InternalState (k0 `xor` 0x736f6d6570736575)
                                       (k1 `xor` 0x646f72616e646f6d)
                                       (k0 `xor` 0x6c7967656e657261)
                                       (k1 `xor` 0x7465646279746573)

doRound (InternalState v0 v1 v2 v3) = do
    let v0'    = v0 + v1
        v2'    = v2 + v3
        v1'    = v1 `rotateL` 13
        v3'    = v3 `rotateL` 16
        v1''   = v1' `xor` v0'
        v3''   = v3' `xor` v2'
        v0''   = v0' `rotateL` 32
        v2''   = v2' + v1''
        v0'''  = v0'' + v3''
        v1'''  = v1'' `rotateL` 17
        v3'''  = v3'' `rotateL` 21
        v1'''' = v1''' `xor` v2''
        v3'''' = v3''' `xor` v0'''
        v2'''  = v2'' `rotateL` 32
     in InternalState v0''' v1'''' v2''' v3''''

runRounds n st = foldl' (const . doRound) st [0..(n-1)]

process c istate m = newState
    where newState = postInject $ runRounds c $ preInject $ istate
          preInject  (InternalState v0 v1 v2 v3) = InternalState v0 v1 v2 (v3 `xor` m)
          postInject (InternalState v0 v1 v2 v3) = InternalState (v0 `xor` m) v1 v2 v3

finish d istate = getDigest $ runRounds d $ preInject istate
    where getDigest (InternalState v0 v1 v2 v3) = SipHash (v0 `xor` v1 `xor` v2 `xor` v3)
          preInject (InternalState v0 v1 v2 v3) = InternalState v0 v1 (v2 `xor` 0xff) v3
