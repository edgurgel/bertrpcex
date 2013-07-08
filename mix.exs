defmodule BertrpcEx.Mixfile do
  use Mix.Project

  def project do
    [ app: :bertrpcex,
      version: "0.0.1",
      elixir: "~> 0.9.3",
      deps: deps(Mix.env) ]
  end

  # Configuration for the OTP application
  def application do
    [ applications: [ :crypto,
                      :poolboy,
                      :exlager],
      env: [
        pools: [
          {:ext, [size: 10,
                  max_overflow: 20],
              [host: {127,0,0,1},
               port: 8000]}
          ]
      ],
    mod: { BertrpcEx, [] }]
  end

  defp deps(:dev) do
    [ {:poolboy, github: "devinus/poolboy", tag: "1.0.0"},
      {:bertex, github: "edgurgel/bertex", tag: "1.1.0"},
      {:exlager, %r".*", github: "khia/exlager"} ]
  end

  defp deps(:test) do
    deps(:dev) ++
     [ {:meck, github: "eproxus/meck", tag: "0.7.2" } ]
  end

end
