shitter
===

A Haskell Twitter shitposting framework.

(c) 2016 Gatlin Johnson. For license information see the `LICENSE` file
distributed with this source code. Like anyone gives a shit.

0. What?
---

This is an (in development) library for interacting with the Twitter
API. The `Shitpost` monad is defined which automatically keeps track of your API
credentials, manages connections, and handles all the bullshit OAuth stuff.

**Update**: I've actually decided to make this more generalized and include
Tumblr as well!

It uses my [tubes][tubes] library for efficient stream handling and great
justice:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString (ByteString)
import Net.Monad.Shitpost
import Tubes

creds :: Credentials
creds = Credentials {
    consumerKey = "some-key",
    consumerSecret = "some-secret",
    token = Just "access-token",
    tokenSecret = Just "access-token-secret"
}

ripHarambe :: Shitpost ()
ripHarambe = do
    status <- tweet "#RipHarambe"
    liftIO . putStrLn $ case statusCode status of
        200 -> "Shit: posted"
        _   -> "Another time, sweet prince"

findHarambeTweets :: Shitpost ()
findHarambeTweets = publicStream ["harambe"] $ \status tweets ->
    runTube $ sample tweets >< pour tweetSinkOfSomeKind

tweetSinkOfSomeKind :: Sink Shitpost ByteString
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
