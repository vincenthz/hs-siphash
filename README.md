Haskell Siphash
===============

Simple module to compute the [SipHash](http://131002.net/siphash/siphash.pdf)
algorithm.

Install
-------

    cabal install siphash

Usage
-----

siphash-2-4:

    import Crypto.MAC.SipHash (hash)
    import qualified Data.ByteString.Char8 as B
    
    k0 = 0xaaaaaaaaaaaaaaaa
    k1 = 0xbbbbbbbbbbbbbbbb
    tag = hash (SipKey k0 k1) (B.pack "my text to hash")

siphash-c-d:

    import Crypto.MAC.SipHash (hash)
    import qualified Data.ByteString.Char8 as B
    
    k0 = 0xaaaaaaaaaaaaaaaa
    k1 = 0xbbbbbbbbbbbbbbbb
    tag = hashWith nbCompressionRounds nbDigestRounds (SipKey k0 k1) (B.pack "my text to hash")
