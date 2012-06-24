{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Monad

import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Test.QuickCheck.Test
import Test.Framework.Providers.QuickCheck2 (testProperty)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B
import Crypto.MAC.SipHash

assertEq expected got
	| expected == got = True
	| otherwise       = error ("expected: " ++ show expected ++ " got: " ++ show got)

vectors =
    [ ( SipKey 0x0706050403020100 0x0f0e0d0c0b0a0908
      , "\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0a\x0b\x0c\x0d\x0e"
      , SipHash 0xa129ca6149be45e5
      )
    ]

katTests v = map (testProperty "kat" . makeTest) v
    where makeTest (key,msg,tag) = assertEq tag $ hash key msg

tests =
    [ testGroup "KAT" $ katTests vectors
    ]

main = defaultMain tests
