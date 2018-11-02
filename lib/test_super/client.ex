defmodule TestSuper.Client do
  @moduledoc """
  Module providing client-side functions for `GenServer`.
  """

  alias SPARQL.Client
  alias SPARQL.Query.Result

  @priv_dir "#{:code.priv_dir(:test_super)}"
  @queries_dir @priv_dir <> "/queries/"
  @query_file "dbpedia_query.rq"
  @service "http://dbpedia.org/sparql"

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

  @doc """
  Store SPARQL query results for `GenServer` with process ID `pid`.
  """
  def putq(pid) do
    {key, value} = _do_query()
    IO.puts "#{inspect key} => #{inspect value}"
    GenServer.cast(pid, {:put, key, value})
  end

  @doc """
  Return default SPARQL query used for demo.
  """
  def query do
    File.read!(@queries_dir <> @query_file)
  end

  @doc """
  Return default SPARQL endpoint used for demo.
  """
  def service, do: @service

  @doc false
  defp _do_query do
    rand = Integer.to_string(Enum.random(1..50_000))
    q = String.replace(query(), "12345", rand)

    {:ok, result} = Client.query(q, @service)

    if result.results == [] do
      {rand, "(not found)"}
    else
      id = result
           |> Result.get(:id)
           |> List.first

      s = result
          |> Result.get(:s)
          |> List.first

      label = result
              |> Result.get(:label)
              |> List.first

      topic = result
              |> Result.get(:topic)
              |> List.first

      {id.value, {s.value, label.value, topic.value}}
    end
  end
end
