defmodule TestSuper do
  @moduledoc """
  Top-level module used in "Robust compute for RDF queries".
  """

  alias TestSuper.Server

  @doc """
  Constructor for new `GenServer` (with no supervision).

  Returns process ID for `GenServer`.
  """
  def genserver do
    opts = [
    ]
    case Server.start_link(opts) do
      {:ok, pid} ->
        IO.puts "TestSuper.Server is starting ... #{inspect pid}"
        pid
      {:error, reason} ->
        IO.puts "! Error: #{inspect reason}"
    end
  end

  @doc """
  Constructor for new `Supervisor` (with `GenServer`).

  Returns process ID for `Supervisor`.
  """
  def supervisor do
    opts = [
      name: TestSuper.Supervisor,
      strategy: :one_for_one
    ]

    case TestSuper.Supervisor.start_link(opts) do
      {:ok, pid} ->
        [{_, child_pid, _, _}] = Supervisor.which_children(pid)
        IO.puts "TestSuper.Supervisor is starting ... #{inspect pid}"
        IO.puts "TestSuper.Server is starting ... #{inspect child_pid}"
        pid
      {:error, reason} ->
        IO.puts "! Error: #{inspect reason}"
    end
  end
end
