stockfighter-gun: yet another Haskell stockfighter library
==========================================================

There are many like it, but this one is mine.

https://github.com/cnr/stockfighter-hs looks a lot more complete and I
recommend you use that instead. My library predates that one, and the
only reason I continued to use it is because I had trouble getting the
other one to build.

``stockfighter-gun`` isn't complete, but what is here should work
correctly. So far I haven't implemented anything to access orderbooks
or the tape. The order of arguments is probably suboptimal (I ended up
writing a lot of lambdas when I used it).

Definition of all the types used in a Stockfighter program (orders,
requests, fills) is in ``Network.Stockfighter.Types``. Actual API
calls are in ``Network.Stockfighter.Requests``. These require a
``StockfighterEnvironment``, which you can make using
``withStockfighterEnvironment`` in ``Network.Stockfighter``. Lenses on
the types are available in ``Network.Stockfighter.Lens``, if you're
into that sort of thing.

There's an example program in `src/stockfighter.hs
<https://github.com/glasserc/stockfighter-gun/blob/master/src/stockfighter.hs>`_.
