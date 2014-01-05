BTC-E web API library for Haskell
=================================

This is an alpha-stage Haskell library for using the [BTC-E Public [Web] API v3](https://btc-e.com/api/3/documentation) for interacting with [BTC-E](https://btc-e.com/).

It uses AutoNonce<sup>TM</sup> (I got the idea from [the BtcE.configure method in therussianphysicist's btc-trader library](https://github.com/therussianphysicist/btc-trader/blob/master/btce.rb)). Most other libraries use one of the following strategies:
* They use a wall clock. This allows only one request per tick and ruins preexisting API keys when the clock overflows Word32.
* They store the last-used nonce in a file. Why would a library-using programmer want to burden their users with that?

The BTC-E web API tells us the highest-ever-used Word32 nonce for an API key if we use a lower nonce, so we can avoid the mistakes above. We follow this strategy:
* We always start off with a nonce of 1 (0 is invalid).
* We let BTC-E correct the nonce when needed.
* We increment the nonce by only 1 for each request.

This gives us some benefits over most libraries:
* We can make requests as quickly as we want.
* We exhaust the nonce space with optimal sluggishness.
* We can automatically pick up right where another library left an API key with a low chance of interfering with the other library.
* We can handle another library using the API key at the same time.

Alpha-stage warnings
====================

* NO WARRANTY WHATSOEVER, including but not limited to monetary losses.
* Assume that the Haskell API may change at any time.
* Assume that some random functions have been written without testing them at all.
* Assume that some BTC-E web API features are missing in the Haskell API.
* Assume that the results of some BTC-E web API features differ from the corresponding Haskell API features (this will be true in production versions as well for badly designed methods).

License
=======

This library is covered by LGPL 3 ([GNU Lesser General Public License v3.0](https://www.gnu.org/licenses/lgpl-3.0-standalone.html)). Due to the possibility of GNU getting new owners via legal actions or other means, there is currently no permission to use any other version of the LGPL or any version of the GPL other than 3.0.

However, to allow license updates, contributions (pull requests, direct pushes, and all other forms of contribution) to this repository will be considered to be licensed to Olathe under LGPL 3 and any newer versions of the LGPL, though I will redistribute them under only LGPL 3 until a license update is required.

API reference
=============

Apologies, but no API reference yet: still hammering out the API, which is in alpha stage, is very ugly, and smells funny.

Thanks
======

Thanks especially to the following:
* [Freenode's #haskell](irc://irc.freenode.net/haskell) for all their assistance in figuring out how to do things in Haskell.
* [Anonymous Haskell code](http://pastebin.com/AfDt8jcs) for giving me an idea of what Haskell libraries to use for the encryption and HTTPS communication.
* [therussianphysicist's btc-trader](https://github.com/therussianphysicist/btc-trader), written in Ruby, for the autononcing idea and for allowing me to compare this library's encryption output to a working library's.
* [The BTC-E Public API v3 documentation](https://btc-e.com/api/3/documentation) and Chrome's translation capabilities.
