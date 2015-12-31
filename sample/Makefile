PROJECT = sample

DEP_PLUGINS = elixir.mk jorel
BUILD_DEPS = elixir.mk jorel

dep_elixir.mk = git https://github.com/botsunit/elixir.mk.git master
dep_jorel = git https://github.com/emedia-project/jorel.git master

ELIXIR_MIX_ENV = test
ELIXIR_DEPS = poison 
dep_poison = git https://github.com/devinus/poison.git master

include erlang.mk

dev: deps app
	@erl -pa ebin include deps/*/ebin deps/*/include deps/elixir/lib/*/ebin

