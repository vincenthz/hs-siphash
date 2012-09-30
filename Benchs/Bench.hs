{-# LANGUAGE BangPatterns #-}
module Main where

import qualified Data.ByteString as B
import qualified Crypto.MAC.SipHash as SipHash
import Criterion.Main

main = do
    let !bs5 = B.pack [0..4]
        !bs8 = B.pack [0..7]
        !bs11 = B.pack [0..10]
        !bs40 = B.pack [0..39]
        !bs1Mb = B.pack . map fromIntegral $ [0..999999::Int]
    let !k = SipHash.SipKey 0 0
    let !hash = SipHash.hash k
    defaultMain
        [ bgroup "Hash"
            [ bench "5" $ whnf hash bs5
            , bench "8" $ whnf hash bs8
            , bench "11" $ whnf hash bs11
            , bench "40" $ whnf hash bs40
            , bench "2^20" $ whnf hash bs1Mb
            ]
        ]
