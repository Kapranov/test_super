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

```bash
iex> IO.puts ExChalk.red("Hello world!")

iex> "hello"
       |> ExChalk.gray
       |> ExChalk.bg_cyan
       |> ExChalk.italic
       |> IO.puts

iex> "Hello world!"
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
        # Process.register(pid, __MODULE__)
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

## GenServer by storing SPARQL results

Let's add some support now to our `TestSuper.Client` module for querying
DBpedia. First, as in previous posts, we'll store a SPARQL query
`dbpedia_query.rq` in our `/priv/queries/` directory.

```bash
mkdir -p priv/queries/
touch priv/queries/dbpedia_query.rq
```

```
# priv/queries/dbpedia_query.rq
prefix dbo: <http://dbpedia.org/ontology/>
prefix foaf: <http://xmlns.com/foaf/0.1/>
prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>

select *
where {
  bind (12345 as ?id)
  ?s dbo:wikiPageID ?id .
  optional { ?s foaf:isPrimaryTopicOf ?topic }
  optional { ?s rdfs:label ?label }
  filter (langMatches(lang(?label), "en"))
} limit 1
```

The `query/0` function just reads the query from the `priv/queries/`
directory. We can try this out.

```bash
iex> query |> IO.puts
#=> prefix dbo: <http://dbpedia.org/ontology/>
    prefix foaf: <http://xmlns.com/foaf/0.1/>
    prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>

    select *
    where {
      bind (12345 as ?id)
      ?s dbo:wikiPageID ?id .
      optional { ?s foaf:isPrimaryTopicOf ?topic }
      optional { ?s rdfs:label ?label }
      filter (langMatches(lang(?label), "en"))
    } limit 1

    :ok
```

This query will look for a resource with a `dbo:wikiPageID` of `12345`.
It will add in `foaf:isPrimaryTopicOf` and `rdfs:label` properties if
present, and filter for English-language labels.

```elixir
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
    # rewrite query with new random wikiPageID
    rand = Integer.to_string(Enum.random(1..50_000))
    q = String.replace(query(), "12345", rand)

    # send query
    {:ok, result} = Client.query(q, @service)

    # parse query result set
    # it's not correct for an elixir also not a good practice
    # if length(result.results) == 0 do
    # that is correct
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
```

So here we have the first function client call `putq/0` which will send
the results from a query to a `GenServer` for storing.

The second (private) function `_do_query/0` does the actual querying and
parses the result set. It first generates a new random `dbo:wikiPageID`
and replaces that ID in the query string. The modified query is sent to
the DBpedia SPARQL endpoint using `SPARQL.Client.query/2`. The result
set is tested and if empty a tuple `{rand, "(not found)"}` is returned.
Otherwise the `?id`, `?s`, ? `label` and `?topic` variables are parsed
out of the first result and returned in the tuple:
`{id.value, {s.value, label.value, topic.value}}`.

We can try this out in IEx.

```bash
pid = genserver
#=> TestSuper.Server is starting ... #PID<0.285.0>
    #PID<0.285.0>

putq(pid)
#=> 19167 => {"http://dbpedia.org/resource/Manuscript",
    "Manuscript", "http://en.wikipedia.org/wiki/Manuscript"}
    :ok

get(pid)
#=> %{
      19167 => {"http://dbpedia.org/resource/Manuscript", "Manuscript",
      "http://en.wikipedia.org/wiki/Manuscript"},
      "13701" => "(not found)"
    }

get(pid, 19167)
#=> {"http://dbpedia.org/resource/Manuscript", "Manuscript",
     "http://en.wikipedia.org/wiki/Manuscript"}
```

Or for querying (and storing) in bulk:

```bash
for _ <- 1..5, do: putq(pid)
#=> "47144" => "(not found)"
    "4144" => "(not found)"
    48913 => {"http://dbpedia.org/resource/Caroline_of_Brunswick",
    "http://en.wikipedia.org/wiki/Caroline_of_Brunswick"}
    38121 => {"http://dbpedia.org/resource/Peperomia",
    "Peperomia", "http://en.wikipedia.org/wiki/Peperomia"}
    "28041" => "(not found)"
    [:ok, :ok, :ok, :ok, :ok]
```

And we can get this back as:

```bash
get(pid)
#=> %{
      19167 => {"http://dbpedia.org/resource/Manuscript", "Manuscript",
      "http://en.wikipedia.org/wiki/Manuscript"},
      38121 => {"http://dbpedia.org/resource/Peperomia", "Peperomia",
      "http://en.wikipedia.org/wiki/Peperomia"},
      48913 => {"http://dbpedia.org/resource/Caroline_of_Brunswick",
      "Caroline of Brunswick",
      "http://en.wikipedia.org/wiki/Caroline_of_Brunswick"},
      "13701" => "(not found)",
      "28041" => "(not found)",
      "4144" => "(not found)",
      "47144" => "(not found)"
    }
```

Note that this includes both the single result we stored earlier
(`19167`), as well as the bulk query results (`38121`, `48913`, `13701`,
`28041`, `4144`, `47144`).

Now we can again inspect the state stored in the `GenServer` process
with the `observer` tool: `:observer.start`

If we choose the 'Processes' tab, sort the processes by clicking on the
'Pid' heading , and select the `GenServer` process  we just created
(`#PID<0.285.0>`) we have the following screen. Double clicking on this
process and choosing the 'State' tab gives us this screen showing the
state. And if we click the link 'Click to expand above term' we get
this:

So, that's a `GenServer` process storing key/value state for the results
from our SPARQL queries.

## Warning on the use of length(x) == 0 by Credo

A good error message says what went wrong, why and how to fix it. So
what is missing on this error message is exactly why `length(...) == 0`
is a bad practice. Here is what I would use:

I've also thought about it. It is also not a good practice, but I guess
the point is that `length(x) == 0` is something people are very used to
do in other languages, on which this isn't a bad practice, since `[]`
are not lists and yes arrays - is that correct?

`length(x) == 0` is a common pattern that says "if the list is empty".
Lists with one element can still be checked by matching on `[_]` more
efficiently, but then when do we stop? Do we say that `length(x) == 2`
can be optimized by doing `[_, _]`? I think `length(x) == 0` makes sense
because it's simple, common, and you can replace it with `x == []`
instead of pattern matching.

There's also a slight change in semantics that has to be considered.
`length(x) == 0` will fail on improper lists, while `x == []` will just
return false. For example:

```elixir
def foo(x) when length(x) == 0 or hd(x) == nil, do: :nothing
def foo(_), do: :something

def bar(x) when x == [] or hd(x) == nil, do: :nothing
def bar(x), do: :something

# Gives different results in some situations:

foo([nil | nil]) #=> :something
bar([nil | nil]) #=> :nothing
```

And even tough, I guess it continues to be a bad practice anyways, if
they use it expecting this kind of behavior, they should change it to
something like:

```elixir
def bar(x) when x == [] or hd(x) == nil, do: :nothing
def bar(x) when tl(x) == nil, do: :something
def bar(x), do: :something
```

I wouldn't expect to see that kind of code, but the behaviour is there
currently. I guess a warning is fine, but that's one of the reasons why
the `length` check can't be optimised by the compiler.

Yep... At least not replacing them by `list == []`.

There is actually ways to optimize, creating functions like
`length_equals?` that traverse the items, but just while it needs to,
and imitating the behaviour of length for malformed lists. But I guess
that's not the intention for now, and I don't know if we can consider
it a clean solution.

## Add in a static supervision tree

At this point we want to add in a supervision tree. Let's first create
ourselves a `TestSuper.Supervisor` module.

```elixir
# lib/test_super/supervisor.ex
defmodule TestSuper.Supervisor do
  @moduledoc """
  Module providing server-side functions for `Supervisor`.
  """

  use Supervisor

  ## Constructor

  def start_link(opts \\ []) do
    Supervisor.start_link(__MODULE__, [], opts)
  end

  ## Callbacks

  def init([]) do
    children = [
      TestSuper.Server
    ]

    opts = [
      name: TestSuper.Supervisor,
      strategy: :one_for_one
    ]

    Supervisor.init(children, opts)
  end
end
```
So, we also want a more friendly means to start this up. Let's add this
to the main `TestSuper` module.

```elixir
# lib/test_super.ex
defmodule TestSuper do
  @moduledoc """
  Top-level module used in "Robust compute for RDF queries".
  """

  # ...

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
```

All this does is to call through to our
`TestSuper.Supervisor.start_link/1` constructor function passing a
couple options: a name and a supervisaion strategy. There are three
defined supervision strategies (see the `Supervisor` documentation for
more info) but we are going to choose the simplest: `:one_for_one`,
i.e. one child process (the `GenServer`) for our `Supervisor`.

We also test the success of this operation and deal accordingly. And
lastly we write out a report of the `Supervisor` and `GenServer`
processes starting up together with their process IDs. (We use the
`Supervisor.which_children/1` function to get the child process ID.)

So, we can use the `supervisor/0` constructor to create a new
`Supervisor` with an attached `GenServer`. But we can simplify further
and get the `Application` itself to start up the `Supervisor`
automatically.

Now let's have a quick peek at that `TestSuper.Application` module.
The one public function `start/2` calls a private function `_start/3`
with a boolean argument which selects for a dynamic supervision tree
on `true` and a static supervision tree on `false`. The intial setting
is `false`, i.e. selects for a static supervision tree.

```elixir
# lib/test_super/application.ex
defmodule TestSuper.Application do
  @moduledoc """
  Module providing the `Application` start function.
  """

  use Application

  @doc """
  Application `start/2` function calls`_start/3` with boolean `flag`.

  The boolean `flag` arg on the `_start/3` call selects a dynamic supervision
  tree on `true`, and a static supervision tree on `false`. Initial setting
  is `false`, i.e. selects for a static supervision tree.
  """
  def start(type, args) do
    _start(type, args, false)
  end

  @doc false
  defp _start(type, args, flag) do
    case flag do
      false -> _static_start(type, args)
      true -> _dynamic_start(type, args)
    end
  end

  @doc false
  defp _static_start(_type, _args) do
    children = [
      TestSuper.Server
    ]

    opts = [
      name: TestSuper.Supervisor,
      strategy: :one_for_one
    ]

    Supervisor.start_link(children, opts)
  end

  @doc false
  defp _dynamic_start(_type, _args) do
    opts = [
      name: TestSuper.DynamicSupervisor,
      strategy: :one_for_one
    ]

    DynamicSupervisor.start_link(opts)
  end
end
```

I won't say any more about the `Application` behaviour here other than
to note that they are process bundles having similar functionality. Our
main reason for using the `Application` to start up the `Supervisor` is
that we can then use the `Observer` to view our `Supervisor` and its
`GenServer`.

```bash
bash> make all

iex> :observer.start
```

Now if we choose the  'Application' tab in the `Observer` we'll see this
and see at left the list of applications being used here. Our
`test_super` application has a couple processes which are linked and
link through to the `Supervisor` and its attached `GenServer`.

If we now double click the `GenServer` process button
`Elixir.TestSuper.Server.<0.243.0>` we will open up in the 'Process
information' tab. And if we select the 'State' tab we'll get this.

We can again try adding state as before and viewing here. (Note that the
panel does not refresh and you'll need to close and reopen to view
changes.)

Now, since the `GenServer` process was auto-started by the `Application`
we're going to have to create a process ID for our client functions
which we can do with the `pid/3` function:

```bash
<0.240.0> -> <0.241.0> -> Elixir.TestSuper.Supervisor -> Elixir.TestSuper.Server.<0.243>

iex> Process.alive?(pid(0,240,0))
#=> true
iex> Process.alive?(pid(0,241,0))
#=> true
iex> Process.whereis Elixir.TestSuper.Supervisor
#=> #PID<0.242.0>
iex> Process.alive?(pid(0,242,0))
#=> true
iex> Process.whereis :"Elixir.TestSuper.Server.<0.243.0>"
#=> #PID<0.243.0>
iex> Process.alive?(pid(0,243,0))
#=> true
iex> pid = pid(0,243,0)
#=> true
iex> Proccess.info(pid)
iex> pid = pid(0,243,0)
#=> #PID<0.243.0>
```
And now we can send requests to the `GenServer`:

```bash
iex> putq(pid)
#=> "46807" => "(not found)"

iex> putq(pid)
#=> "11546" => "(not found)"

iex> putq(pid)
#=> 35621 => {"http://dbpedia.org/resource/2_Timothy", "2 Timothy",
    "http://en.wikipedia.org/wiki/2_Timothy"}
    :ok

iex> get(pid)
iex> get(pid, 35621)
iex> for _ <- 1..5, do: putq(pid)
iex> get(pid)
```

And if we close and repoen the 'State' tab we'll see the new state we
just stored.

So, what's the `Supervisor` doing for us? Let's see.

If we go back to the 'Applications' tab on the main window and now right
click on the `GenServer` process, we'll get this popup.

So, choose 'Kill process' and this dialog box pops up. Just click 'OK',
and ...

Look at that. It's the same as before (almost), but now the `GenServer`
process has been automatically restarted (albeit with a different
process ID, `#PID<0.3961.0>`  instead of `#PID<0.243.0>` ).

Now if there was any state stored in the process then that will have
been erased. We're back to a blank slate. We need other strategies to
preserve the state. But the process itself survives, and any compute
functions associated with the `GenServer` are again available. Good as
new.

## Add in a dynamic supervision tree

Now the on current example was for a static supervision tree. We could have
added more `GenServer` proceeses under a single `Supervisor`, or even
built up a tree with many levels by adding `Supervisor` processes under
a `Supervisor`.

But now let's look at how to create a dynamic supervision tree. Let's
first create ourselves a `TestSuper.DynamicSupervisor` module.

```bash
touch lib/test_super/dynamic.ex
```

```elixir
# lib/test_super/dynamic.ex
defmodule TestSuper.DynamicSupervisor do
  @moduledoc """
  Module providing server-side functions for `DynamicSupervisor`.
  """

  use DynamicSupervisor

  ## Constructor

  @doc """
  Constructor for `DynamicSupervisor`.
  """
  def start_link(opts \\ []) do
    DynamicSupervisor.start_link(__MODULE__, [], opts)
  end

  ## Callbacks

  @doc """
  `DynamicSupervisor` callback `init/1`.
  """
  def init([]) do
    opts = [
      name: TestSuper.DynamicSupervisor,
      strategy: :one_for_one
    ]

    DynamicSupervisor.init(opts)
  end
end
```

And again we also want a more friendly means to start this up. Let's
add this to the main `TestSuper` module.


```elixir
@doc """
Constructor for new `DynamicSupervisor`.

Returns process ID for `DynamicSupervisor`.
"""
def supervisor_d() do
  opts = [
    name: TestSuper.DynamicSupervisor,
    strategy: :one_for_one
  ]

  case TestSuper.DynamicSupervisor.start_link(opts) do
    {:ok, pid} ->
      IO.puts "TestSuper.DynamicSupervisor is starting ... #{inspect pid}"
      pid
    {:error, reason} ->
      IO.puts "! Error: #{inspect reason}"
  end
end
```

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

  @doc """
  Constructor for new `GenServer` (with `DynamicSupervisor`).

  Returns process ID for `GenServer`.
  """
  def genserver_d do
    case DynamicSupervisor.start_child(
        TestSuper.DynamicSupervisor, TestSuper.Server
      ) do
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
```

This `supervisor_d/0` constructor function is pretty much the same as
the `supervisor/0` constructor.

But again we can just use the `Application` to start up our
`DynamicSupervisor` process. For now let's just flip the `false`arg in
the `TestSuper.Application` module to `true` on the `start/2` function
to read as:

```elixir
defmodule TestSuper.Application do
  @moduledoc """
  Module providing the `Application` start function.
  """

  use Application

  @doc """
  Application `start/2` function calls`_start/3` with boolean `flag`.

  The boolean `flag` arg on the `_start/3` call selects a dynamic supervision
  tree on `true`, and a static supervision tree on `false`. Initial setting
  is `false`, i.e. selects for a static supervision tree.
  """
  def start(type, args) do
    _start(type, args, true)
  end

  # ...
end
```
And let's restart IEx.

```bash
bash> make all

iex> :observer.start
```

Now this time we only have the `DynamicServer`. There are no child
`GenServer` processes attached.

Let's add a `GenServer`.

```bash
<0.249.0> -> <0.250.0> -> Elixir.TestSuper.DynamicSupervisor

iex> iex(2)> Process.whereis Elixir.TestSuper.DynamicSupervisor
#=> #PID<0.251.0>

iex> pid = pid(0,251,0)
#=> #PID<0.251.0>

iex> Process.info(pid)

iex> genserver_d
#=> #PID<0.272.0>
```

Now we can see this atached. And, in fact, this looks very similar to
the previous static supervision tree example.


```bash
<0.240.0> -> <0.241.0> -> Elixir.TestSuper.DynamicSupervisor -> Elixir.TestSuper.Server.<0.272.0>
```

Let's add another `GenServer`.

```bash
iex> genserver_d
#=> #PID<0.284.0>

<0.240.0> -> <0.241.0> -> Elixir.TestSuper.DynamicSupervisor
                                          |
                                          V
                          Elixir.TestSuper.Server.<0.272.0>
                          Elixir.TestSuper.Server.<0.284.0>
```

And now we can see two child `GenServer` processes for the
`DynamicSupervisor`.

Let's go crazy and add ten `GenServer` child processes.

```bash
iex> for _ <- 1..10, do: genserver_d
#=> [#PID<0.297.0>, #PID<0.298.0>, #PID<0.299.0>,
     #PID<0.300.0>, #PID<0.301.0>, #PID<0.302.0>,
     #PID<0.303.0>, #PID<0.304.0>, #PID<0.305.0>,
     #PID<0.306.0>]

<0.240.0> -> <0.241.0> -> Elixir.TestSuper.DynamicSupervisor
                                          |
                                          V
                          Elixir.TestSuper.Server.<0.272.0>
                          Elixir.TestSuper.Server.<0.284.0>
                          Elixir.TestSuper.Server.<0.297.0>
                          Elixir.TestSuper.Server.<0.298.0>
                          Elixir.TestSuper.Server.<0.299.0>
                          Elixir.TestSuper.Server.<0.300.0>
                          Elixir.TestSuper.Server.<0.301.0>
                          Elixir.TestSuper.Server.<0.302.0>
                          Elixir.TestSuper.Server.<0.303.0>
                          Elixir.TestSuper.Server.<0.304.0>
                          Elixir.TestSuper.Server.<0.305.0>
                          Elixir.TestSuper.Server.<0.306.0>
```

Yup.

That's right. The `DynamicSupervisor` here is managing a dozen
`GenServer` processes, but could just as easily be twelve thousand,
or more.


I've shown here how to manage fault tolerance in Elixir with
supervision trees. We first reviewed the process model for Erlang
(Elixir) and then discussed some of the abstraction patterns that OTP
provides for processes: agents, servers, supervisors, and tasks.

We then developed a demo to show how the `GenServer` process can be
used to store state. In this example we used a map to maintain a
key/value store. We also made use of the `SPARQL.Client.ex` package
to query DBpedia and to save our results into the `GenServer`.

We then showed how both static and dynamic supervision trees may be
created for managing child processes using the `Supervisor` and
`DynamicSupervisor` patterns. Using the `Observer` we showed how child
processes can be killed and get restarted automatically by the
`Supervisor`.

It is this support for fault tolerance and distributed compute that
makes Elxir such a fascinating candidate for semantic web applications.

### 3 November 2018 by Oleg G.Kapranov

[1]: https://www.manning.com/books/elixir-in-action
[2]: https://github.com/brianstorti/elixir-registry-example-chat-app
[3]: https://medium.com/@StevenLeiva1/elixir-process-registries-a27f813d94e3
[4]: https://www.brianstorti.com/process-registry-in-elixir/
[5]: https://github.com/boydm/scenic
[6]: https://github.com/boydm/scenic_new
[7]: https://gitlab.com/martinstannard/petri
[8]: https://gitlab.com/martinstannard/turtles
[9]: https://github.com/tonyhammond/examples
