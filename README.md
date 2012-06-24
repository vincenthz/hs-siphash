Haskell Siphash
===============

Simple module to compute the [SipHash](http://131002.net/siphash/siphash.pdf)
algorithm.

Install
-------

    cabal install sighash

Usage
-----

sighash-2-4:

    import Crypto.MAC.SigHash (hash)
    import qualified Data.ByteString.Char8 as B
    
    k0 = 0xaaaaaaaaaaaaaaaa
    k1 = 0xbbbbbbbbbbbbbbbb
    tag = hash (SigKey k0 k1) (B.pack "my text to hash")

sighash-c-d:

    import Crypto.MAC.SigHash (hash)
    import qualified Data.ByteString.Char8 as B
    
    k0 = 0xaaaaaaaaaaaaaaaa
    k1 = 0xbbbbbbbbbbbbbbbb
    tag = hashWith nbCompressionRounds nbDigestRounds (SigKey k0 k1) (B.pack "my text to hash")
