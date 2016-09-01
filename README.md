shitter
===

A Haskell Twitter shitposting framework.

(c) 2016 Gatlin Johnson. For license information see the `LICENSE` file
distributed with this source code. Like anyone gives a shit.

0. What?
---

This is an (in development) library for interacting with the Twitter
API. It uses my [tubes][tubes] library for efficient stream handling and great
justice:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString (ByteString, pack, unpack)
import Net.Monad.Twitter
import Tubes

creds :: Credentials
creds = Credentials {
    consumerKey = "some-key",
    consumerSecret = "some-secret",
    token = Just "access-token",
    tokenSecret = Just "access-token-secret"
}

ripHarambe :: Twitter ()
ripHarambe = do
    status <- tweet "#RipHarambe"
    liftIO . putStrLn $ case statusCode status of
        200 -> "Shit: posted"
        _   -> "Another time, sweet prince"

findHarambeTweets :: Twitter ()
findHarambeTweets = publicStream ["harambe"] $ \status tweets ->
    runTube $ sample tweets >< pour tweetSinkOfSomeKind

tweetSinkOfSomeKind :: Sink Twitter ByteString
tweetSinkOfSomeKind = {- ... -}
```

1. Installation
---

This isn't polluting the Hackage repository (yet) but if you have [stack][stack]
installed then you can:

    $> git clone https://github.com/gatlin/shitter.git shitter
    $> cd shitter
    $> stack setup && stack build && stack install

and then go do your thing.

2. Questions / Comments / Wanna talk about Harambe?
---

Email me at <gatlin@niltag.net> or use the GitHub Issues feature.

[stack]: https://www.haskellstack.org
[tubes]: http://hackage.haskell.org/package/tubes
