defmodule TestSuper.Application do
  @moduledoc """
  Module providing the `Application` start function.
  """

  use Application

  def start(_type, _args) do
    children = []

    opts = [strategy: :one_for_one, name: TestSuper.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
