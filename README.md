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

## Add in a dynamic supervision tree

Now the on current example was for a static supervision tree. We could have
added more `GenServer` proceeses under a single `Supervisor`, or even
built up a tree with many levels by adding `Supervisor` processes under
a `Supervisor`.

```elixir
# lib/test_super/application.ex
defmodule TestSuper.Application do
  @moduledoc """
  Module providing the `Application` start function.
  """

  use Application

  def start(_type, _args) do
    children = [
      TestSuper.Server
    ]

    opts = [strategy: :one_for_one, name: TestSuper.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
```

```bash
bash> make all

iex> :observer.start

<0.240.0> -> <0.241.0> -> Elixir.TestSuper.Supervisor -> Elixir.TestSuper.Server.<0.243>

iex> Process.alive?(pid(0,243,0))
#=> true

iex> Process.whereis TestSuper.Supervisor
#=> #PID<0.242.0>

iex> Process.whereis :"Elixir.TestSuper.Server.<0.243.0>"
#=> #PID<0.243.0>

iex> pid = pid(0,243,0)
#=> true

iex> Proccess.info(pid)

iex> putq(pid)
iex> get(pid)
iex> get(pid, 14119)
iex> for _ <- 1..5, do: putq(pid)
iex> get(pid)
```

### 2 November 2018 by Oleg G.Kapranov

[1]: https://www.manning.com/books/elixir-in-action
[2]: https://github.com/uwiger/gproc
[3]: https://github.com/brianstorti/elixir-registry-example-chat-app
[4]: https://medium.com/@StevenLeiva1/elixir-process-registries-a27f813d94e3
[5]: https://www.brianstorti.com/process-registry-in-elixir/
