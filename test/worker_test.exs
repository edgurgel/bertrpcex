Code.require_file "test_helper.exs", __DIR__

defmodule BertrpcEx.WorkerTest do
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
    :meck.expect(Bertex, :encode, 1, :encoded_data)
    :meck.expect(:gen_tcp, :send, 2, :ok)
    :meck.expect(:gen_tcp, :recv, 3, {:ok, :recv_data})
    :meck.expect(Bertex, :decode, 1, {:reply, :result})
    :meck.expect(:gen_tcp, :close, 1, :ok)
    server_info = BertrpcEx.Worker.ServerInfo.new(host: :host, port: :port, socket: :socket)
    assert handle_call({:module, :func, [:args]}, :pid, server_info) == {:reply, :result, server_info}
    assert :meck.validate :gen_tcp
    assert :meck.validate Bertex
  end

  test "a successful BERTRPC cast" do
    :meck.expect(Bertex, :encode, 1, :encoded_data)
    :meck.expect(:gen_tcp, :send, 2, :ok)
    server_info = BertrpcEx.Worker.ServerInfo.new(host: :host, port: :port, socket: :socket)
    assert handle_cast({:module, :func, [:args]}, server_info) == {:noreply, server_info}
    assert :meck.validate :gen_tcp
    assert :meck.validate Bertex
  end

  test "init passing host and port on args" do
    :meck.expect(:gen_tcp, :connect, 3, {:ok, :socket})
    assert init(host: {127, 0, 0, 1}, port: 8080) == {:ok, BertrpcEx.Worker.ServerInfo.new(host: {127, 0, 0, 1}, port: 8080, socket: :socket)}
    assert :meck.validate :gen_tcp
  end

  test "init missing host or port on args" do
    assert init(host: {127, 0, 0, 1}) == {:stop, {:error, "Host and Port must be defined for each server"}}
    assert init(port: 8080) == {:stop, {:error, "Host and Port must be defined for each server"}}
  end

end
