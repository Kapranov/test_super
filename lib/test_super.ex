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
end
