Code.require_file "test_helper.exs", __DIR__

defmodule BertrpcExTest do
  use ExUnit.Case
  import BertrpcEx.Worker

  setup do
    :meck.new(:gen_tcp, [:unstick])
    :meck.new(Bertex)
  end

  teardown do
    :meck.unload(:gen_tcp)
    :meck.unload(Bertex)
  end

  test "a successful BERTRPC call" do
    :meck.expect(:gen_tcp, :connect, 3, {:ok, :socket})
    :meck.expect(Bertex, :encode, 1, :encoded_data)
    :meck.expect(:gen_tcp, :send, 2, :ok)
    :meck.expect(:gen_tcp, :recv, 3, {:ok, :recv_data})
    :meck.expect(Bertex, :decode, 1, {:reply, :result})
    :meck.expect(:gen_tcp, :close, 1, :ok)
    server_info = BertrpcEx.Worker.ServerInfo.new(host: :host, port: :port)
    assert handle_call({:module, :func, [:args]}, :pid, server_info) == {:reply, :result, server_info}
    assert :meck.validate :gen_tcp
    assert :meck.validate Bertex
  end

  test "a successful BERTRPC cast" do
    :meck.expect(:gen_tcp, :connect, 3, {:ok, :socket})
    :meck.expect(Bertex, :encode, 1, :encoded_data)
    :meck.expect(:gen_tcp, :send, 2, :ok)
    server_info = BertrpcEx.Worker.ServerInfo.new(host: :host, port: :port)
    assert handle_cast({:module, :func, [:args]}, server_info) == {:noreply, server_info}
    assert :meck.validate :gen_tcp
    assert :meck.validate Bertex
  end

end
