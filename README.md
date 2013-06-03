# BERT-RPC Elixir client

A pool of BERT-RPC clients

## Implementation

bertrpcex uses poolboy to have pools of workers that connect/encode/decode BERT-RPC.

The configuration is done directly on each pool, this is part of the .app (as example):

```elixir
{env, [
    {pools, [
      {ext, [
        {size, 10},
        {max_overflow, 20}
        ], [
        {hostname, {127,0,0,1}},
        {port, 8000}
        ]},
      {messages_controller, [
        {size, 5},
        {max_overflow, 10}
        ], [
        {hostname, {127,0,0,1}},
        {port, 9999}
        ]}
      ]}
    ]}
```

This configuration means:

* Module ext is hosted at 127.0.0.1 at 8000 on a pool of size 10 and max_overflow 20
* Module messages_controller is hosted at 127.0.0.1 at 9999 on a pool of size 5 and max overflow 10

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

