defmodule BertrpcEx.Mixfile do
  use Mix.Project

  def project do
    [ app: :bertrpcex,
      version: "0.0.1",
      deps: deps ]
  end

  # Configuration for the OTP application
  def application do
    [ applications: [ :crypto,
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

  defp deps do
    [ {:poolboy, github: "devinus/poolboy", tag: "1.0.0"},
      {:bert, github: "eproxus/bert.erl"},
      {:goldrush, github: "DeadZen/goldrush", tag: "7ff9b03"},
      {:lager, %r(.*), git: "https://github.com/basho/lager.git"},
      {:exlager, %r".*", github: "khia/exlager"} ]
  end
end
