defmodule BertrpcEx.Supervisor do
  use Supervisor.Behaviour

  def start_link do
    :supervisor.start_link(__MODULE__, [])
  end

  def init([]) do
    {:ok, pools} = :application.get_env(:bertrpcex, :pools)
    pool_specs = Enum.map(pools,
      fn({module_list, size_args, worker_args}) ->
        lc m inlist module_list, do: pool_spec(m, size_args, worker_args)
      end)
    supervise(List.flatten(pool_specs), strategy: :one_for_one)
  end

  defp pool_spec(module, size_args, worker_args) do
    pool_args = [ {:name, {:local, module}},
                  {:worker_module, :'Elixir.BertrpcEx.Worker'}] ++ size_args
    :poolboy.child_spec(module, pool_args, worker_args)
  end
end
