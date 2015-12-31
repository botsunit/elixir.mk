PROJECT = test

DEP_PLUGINS = elixir.mk
BUILD_DEPS = elixir.mk

dep_elixir.mk = git https://github.com/botsunit/elixir.mk.git master

ELIXIR_MIX_ENV = test
ELIXIR_DEPS = poison 
dep_poison = git https://github.com/devinus/poison.git master

include erlang.mk

dev: deps app
	@erl -pa ebin include deps/*/ebin deps/*/include deps/elixir/lib/*/ebin

