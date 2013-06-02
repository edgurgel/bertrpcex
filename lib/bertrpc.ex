defmodule BertrpcEx do
  use Application.Behaviour
  require Lager

  # See http://elixir-lang.org/docs/stable/Application.Behaviour.html
  # for more information on OTP Applications
  def start(_type, _args) do
    BertrpcEx.Supervisor.start_link
  end

  def stop(_state) do
    :ok
  end

  def call(module, func, args) do
    :poolboy.transaction(module,
                         fn(worker) -> :gen_server.call(worker, {module, func, args}) end)
  end

  def cast(module, func, args) do
    :poolboy.transaction(module,
                         fn(worker) -> :gen_server.cast(worker, {module, func, args}) end)
  end
end
