defmodule BertrpcEx.Supervisor do
  use Supervisor.Behaviour

  def start_link do
    :supervisor.start_link(__MODULE__, [])
  end

  def init([]) do
    {:ok, pools} = :application.get_env(:bertrpcex, :pools)
    pool_specs = Enum.map(pools,
      fn({name, size_args, worker_args}) ->
        pool_args = [{:name, {:local, name}},
        {:worker_module, :'Elixir.BertrpcEx.Worker'}] ++ size_args
        :poolboy.child_spec(name, pool_args, worker_args)
      end)
    supervise(pool_specs, strategy: :one_for_one)
  end
end
