defmodule TestSuper.Client do
  @moduledoc """
  Module providing client-side functions for `GenServer`.
  """

  @doc """
  Return map stored by `GenServer` with process ID `pid`.
  """
  def get(pid) do
    GenServer.call(pid, {:get})
  end

  @doc """
  Return value for `key` stored by `GenServer` with process ID `pid`.
  """
  def get(pid, key) do
    GenServer.call(pid, {:get, key})
  end

  @doc """
  Return list of keys for `GenServer` with process ID `pid`.
  """
  def keys(pid) do
    GenServer.call(pid, {:keys})
  end

  @doc """
  Store `value` by `key` for `GenServer` with process ID `pid`.
  """
  def put(pid, key, value) do
    GenServer.cast(pid, {:put, key, value})
  end
end
