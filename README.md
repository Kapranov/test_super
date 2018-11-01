# TestSuper - Robust compute for RDF queries

**TODO: Add description**

Let's lay out an outline for the project:

1. We'll initially create the project.
2. We'll then implement a `GenServer`, show some callbacks and a public
   interface for those.
3. We'll next demo the `GenServer` as a basic key/value store.
4. We'll then add SPARQL queries into the mix using `SPARQL.Client` and
   save our results into a `GenServer`.
5. Next we'll put a `GenServer` under a static supervision tree and show
   how it is automatically restarted when it errors.
6. And lastly we'll create `GenServer` processes dynamically.

## Create the project

```bash
mkdir test_super; cd test_super

mix new . --sup
```

## Setup tools

A static code analysis tool, TDD, report tool and automatic recompilation,
easier terminal styling.

```elixir
# mix.exs
defmodule TestSuper.MixProject do
  use Mix.Project

  def project do
    [
      app: :test_super,
      version: "0.1.0",
      elixir: "~> 1.7",
      start_permanent: Mix.env() == :prod,
      test_coverage: [tool: ExCoveralls],
      preferred_cli_env: [
        coveralls: :test,
        "coveralls.detail": :test,
        "coveralls.post": :test,
        "coveralls.html": :test,
        "coveralls.json": :test
      ],
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: applications(Mix.env),
      mod: {TestSuper.Application, []}
    ]
  end

  defp deps do
    [
      {:credo, "~> 0.10.0", only: [:dev, :test], runtime: false},
      {:exchalk, "~> 1.0.2" },
      {:excoveralls, "~> 0.10.1", only: :test},
      {:ex_unit_notifier, "~> 0.1.4", only: :test},
      {:mix_test_watch, "~> 0.9.0", only: :dev, runtime: false},
      {:remix, "~> 0.0.2", only: :dev}
    ]
  end

  defp applications(:dev), do: applications(:all) ++ [:remix]
  defp applications(_all), do: [:logger]
end

# test/test_helper.exs
ExUnit.configure formatters: [ExUnit.CLIFormatter, ExUnitNotifier]
ExUnit.start()

# config/config.exs
use Mix.Config

if Mix.env == :dev do
  config :mix_test_watch, clear: true
  config :remix, escript: true, silent: true
end

if Mix.env == :test do
  config :ex_unit_notifier, notifier: ExUnitNotifier.Notifiers.NotifySend
end

# import_config "#{Mix.env()}.exs"
```

check it out:

```bash
mix deps.get
mix deps.update --all
mix deps.get

mix test

mix credo --strict

mix coveralls
mix coveralls.detail
mix coveralls.html
mix coveralls.json

iex -S mix
```

Easier terminal styling:

```elixir
IO.puts ExChalk.red("Hello world!")

"hello"
  |> ExChalk.gray
  |> ExChalk.bg_cyan
  |> ExChalk.italic
  |> IO.puts

"Hello world!"
  |> ExChalk.red
  |> ExChalk.italic
  |> ExChalk.bg_blue
  |> IO.puts
```

## Create Makefile for running project

```bash
cat << 'EOF' > Makefile
V ?= @
SHELL := /usr/bin/env bash
ERLSERVICE := $(shell pgrep beam.smp)

ELIXIR = elixir

VERSION = $(shell git describe --tags --abbrev=0 | sed 's/^v//')

NO_COLOR=\033[0m
INFO_COLOR=\033[2;32m
STAT_COLOR=\033[2;33m

# ------------------------------------------------------------------------------

help:
			$(V)echo Please use \'make help\' or \'make ..any_parameters..\'

push:
			$(V)git add .
			$(V)git commit -m "added support Makefile"
			$(V)git push -u origin master

git-%:
			$(V)git pull

kill:
			$(V)echo "Checking to see if Erlang process exists:"
			$(V)if [ "$(ERLSERVICE)" ]; then killall beam.smp && echo "Running Erlang Service Killed"; else echo "No Running Erlang Service!"; fi

clean:
			$(V)mix deps.clean --all
			$(V)mix do clean
			$(V)rm -fr _build/ ./deps/

packs:
			$(V)mix deps.get
			$(V)mix deps.update --all
			$(V)mix deps.get

report:
			$(V)MIX_ENV=dev
			$(V)mix coveralls
			$(V)mix coveralls.detail
			$(V)mix coveralls.html
			$(V)mix coveralls.json

test:
			$(V)clear
			$(V)echo -en "\n\t$(INFO_COLOR)Run server tests:$(NO_COLOR)\n\n"
			$(V)mix test

credo:
			$(V)mix credo --strict
			$(V)mix coveralls

all: test

run: kill clean packs
			$(V)iex -S mix

halt: kill
			$(V)echo -en "\n\t$(STAT_COLOR) Run server http://localhost:$(NO_COLOR)$(INFO_COLOR)PORT$(NO_COLOR)\n"
			$(V)mix run --no-halt

start: kill
			$(V)echo -en "\n\t$(STAT_COLOR) Run server http://localhost:$(NO_COLOR)$(INFO_COLOR)PORT$(NO_COLOR)\n"
			$(V)iex -S mix

.PHONY: test halt
EOF

cat << 'EOF' > run.sh
#!/usr/bin/env bash

make start
EOF

chmod 755 ./run.sh

make test
make credo
make report
make start

./run.sh
```

## Packages SPARQL extension

```elixir
# mix.exs
defmodule TestSuper.MixProject do
  use Mix.Project

  # ...

  defp deps do
    [
      {:ex_doc, "~> 0.19.1", only: :dev, runtime: false},
      {:hackney, "~> 1.14.3"},
      {:sparql_client, "~> 0.2.1"},
    ]
  end

  # ...
end

# config/config.exs
use Mix.Config

if Mix.env == :dev do
  config :tesla, :adapter, Tesla.Adapter.Hackney
  # ...
end

 # ...

# import_config "#{Mix.env()}.exs"
```

## Create Modules and Docs

```bash
cat << 'EOF' > .iex.exs
import TestSuper
import TestSuper.Client
import TestSuper.Server
EOF

touch lib/test_super/client.ex
touch lib/test_super/server.ex
```

```elixir
defmodule TestSuper do
  @moduledoc """
  Top-level module used in "Robust compute for RDF queries".
  """
end

defmodule TestSuper do
  @moduledoc """
  Top-level module used in "Robust compute for RDF queries".
  """
end

defmodule TestSuper.Client do
  @moduledoc """
  Module providing client-side functions for `GenServer`.
  """
end

defmodule TestSuper.Server do
  @moduledoc """
  Module providing server-side functions for `GenServer`.
  """
end
```

## Implement a `GenServer`

Recall that a `GenServer` is a process with an OTP behaviour and OTP
defines the server functionality. All we need to do is to add in
client-side functionality via the callbacks. And also, to start up the
process.

So let's now flesh this out.

```elixir
# lib/test_super/server.ex
defmodule TestSuper.Server do
  @moduledoc """
  Module providing server-side functions for `GenServer`.
  """

  use GenServer

  ## Constructor

  @doc """
  Constructor for `GenServer`.
  """
  def start_link(opts \\ []) do
    case GenServer.start_link(__MODULE__, opts) do
      {:ok, pid} ->
        # register process name
        num = String.replace("#{inspect pid}", "#PID", "")
        Process.register(pid, Module.concat(__MODULE__, num))
        {:ok, pid}
      {:error, reason} -> {:error, reason}
    end
  end

  ## Callbacks

  @doc """
  Server callback `init/1`.
  """
  def init(_) do
    {:ok, Map.new}
  end

  @doc """
  Server callback `handle_call/3` for client `get/1`.
  """
  def handle_call({:get}, _from, state) do
    {:reply, state, state}
  end

  @doc """
  Server callback `handle_call/3` for client `get/2`.
  """
  def handle_call({:get, key}, _from, state) do
    {:reply, Map.fetch!(state, key), state}
  end

  @doc """
  Server callback `handle_call/3` for client `keys/0`.
  """
  def handle_call({:keys}, _from, state) do
    {:reply, Map.keys(state), state}
  end

  @doc """
  Server callback `handle_cast/2` for client `put/3`.
  """
  def handle_cast({:put, key, value}, state) do
    {:noreply, Map.put(state, key, value)}
  end
end
```

On the server side, we can implement a variety of callbacks to guarantee
server initialization, termination, and handling of requests. We are
just going to implement these three callbacks for initialization and
request  handling:

* `init/1` - intialize the server
* `handle_call/3` - service a `call/2` client request (for reads)
* `handle_cast/2` - service a `cast/2` client request (for writes)

Since we're going to use this `GenServer` to manage a key/value store
we'll use a `Map` to implement this. This is set up by the `init/1`
callback.

Often the client functions (calls) and server functions (callbacks) will
be placed together in the server module.  Here, however, we have made a
cleaner separation and put our client calls into a `TestSuper.Client`
module and kept the server callbacks in the `TestSuper.Server` module.

Here's our `TestSuper.Client` module calls.

```elixir
# lib/test_super/client.ex
defmodule TestSuper.Client do
  @moduledoc """
  Module providing client-side functions for `GenServer`.
  """

  ## Calls

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
```

This is just for demo purposes so we only define some basic read/write
functions (`get/1`, `get/2` and `put/3`). We don't have any update or
delete functions but they are easy to add.

So far, so good. We just need a more friendly way of starting our
`GenServer`. We'll add this `genserver/0` constructor to our main module
`TestSuper`.

```elixir
# lib/test_super.ex
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
```

We're now all set.

And check out it `iex -S mix` and `:observer.start` and you will see
in section an application:

```bash
<0.240.0> -> <0.241.0> -> <Elixir.TestSuper.Supervisor>

iex> pid = genserver
#=> TestSuper.Server is starting ... #PID<0.268.0>
#=> #PID<0.268.0>

iex> Process.whereis TestSuper.Supervisor
#=> #PID<0.242.0>

iex> Process.alive?(pid(0,240,0)) #=> true
iex> Process.alive?(pid(0,241,0)) #=> true
iex> Process.alive?(pid(0,242,0)) #=> true
iex> Process.alive?(pid(0,268,0)) #=> true

iex> Process.info(pid(0,240,0))
iex> Process.info(pid(0,241,0))
iex> Process.info(pid(0,242,0))
iex> Process.info(pid(0,268,0))
```

## GenServer by storing some key/value pairs

So, let's try this out first with our client API.

```bash
iex> pid = genserver        #=> #PID<0.245.0>
iex> put(pid, :foo, "bar")  #=> :ok
iex> get(pid)               #=> %{foo: "bar"}
iex> put(pid, :baz, 123)    #=> :ok
iex> get(pid)               #=> %{baz: 123, foo: "bar"}
iex> get(pid, :foo)         #=> "bar"
iex> get(pid, :baz)         #=> 123
```

Now we can also inspect the guts of the `GenServer` process with
the `Observer` tool we saw in an earlier post: `:observer.start`

Now if we scoot over to the Processes tab we can locate the `GenServer`
process, by sorting on the Pid or Name column:
`Elixir.TestSuper.Server.<0.245.0>`. This is the name that we registered
for the process. Also, under the Current Function heading we see the
current function identified as `gen_server:loop/7`. So, we are indeed
running a `GenServer` process. Now selecting the State tab, we can see
the actual state stored in the process and it's an empty map. If we now
put a couple key/value pairs as before and again inspect the state we
see now we have a map with entries.

So, that's a `GenServer` process storing key/value state.

### 1 November 2018 by Oleg G.Kapranov
