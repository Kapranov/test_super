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

### 1 November 2018 by Oleg G.Kapranov
