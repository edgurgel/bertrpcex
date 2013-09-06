[![Build Status](https://travis-ci.org/edgurgel/bertrpcex.png?branch=master)](https://travis-ci.org/edgurgel/bertrpcex)

# BERT-RPC Elixir client

A pool of BERT-RPC clients

## Implementation

bertrpcex uses poolboy to have pools of workers that connect/encode/decode BERT-RPC.

The configuration is done directly on each pool, this is part of the .app (as example):

```elixir
env: [
  pools: [
    { [:ext, :nat],
      [size: 10, max_overflow: 20],
      [host: {127,0,0,1}, port: 8000]
    }
  ]
]
```

This configuration means:

Module ext and nat are hosted at 127.0.0.1 at 8000 and 7999 on pools of size 10 and max_overflow 20 each

Usage:

Let's suppose that ext has a binary method named... binary.

```elixir
response = BertrpcEx.call(:ext, :binary, [1,2]).
```

This will pick a worker, execute the call and return the expected response.

That's it.

## TODO

Write tests!

## Contributing

If you'd like to hack on Bertrpc.ex, start by forking my repo on Github.

You need [Elixir](http://www.elixir-lang.org) and Erlang R16.

Dependencies can be fetched running:

```
$ mix deps.get
```

To compile:

```
$ mix compile
```

Pull requests are greatly appreciated.

