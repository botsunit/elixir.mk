# Copyright (c) 2015, Bots Unit<br />
# All rights reserved.
# 
# Redistribution and use in source and binary forms, with or without 
# modification, are permitted provided that the following conditions are met:
# 
# 1. Redistributions of source code must retain the above copyright notice, 
#    this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright notice, 
#    this list of conditions and the following disclaimer in the documentation 
#    and/or other materials provided with the distribution.
# 
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE 
# DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE 
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL 
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR 
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER 
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, 
# OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE 
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

ELIXIR_URL ?= https://github.com/elixir-lang/elixir.git
ELIXIR_BRANCH ?= master
ELIXIR_MIX_ENV ?= prod

ELIXIR_BIN = $(DEPS_DIR)/elixir/bin
ALL_ELIXIR_DEPS_DIRS = $(addprefix $(DEPS_DIR)/,$(ELIXIR_DEPS))
ALL_ELIXIR_DEPS_D = $(addsuffix /elixir_deps.mk,$(ALL_ELIXIR_DEPS_DIRS))
dep_elixir = git $(ELIXIR_URL) $(ELIXIR_BRANCH)

# Verbosity.

ex_verbose_0 = @echo " EX    " $(filter %.ex,$(?F));
ex_verbose = $(ex_verbose_$(V))

# Core targets.

EX_FILES = $(sort $(call core_find,src/,*.ex))

ifneq ($(EX_FILES),)

BEAM_FILES += $(addprefix ebin/,$(patsubst %.ex,%.beam,$(notdir $(EX_FILES))))

# Rebuild EX modules when the Makefile changes.
$(EX_FILES): $(MAKEFILE_LIST) $(DEPS_DIR)/elixir
	@touch $@

ebin/$(PROJECT).app:: $(EX_FILES) | ebin/
	$(ex_verbose) $(ELIXIR_BIN)/elixirc $(EX_FILES) -o ebin/ 

endif

# Elixir deps

deps:: elixir-deps

define elixir
MIX_ENV=$(ELIXIR_MIX_ENV) $(ELIXIR_BIN)/elixir -e "$(subst $(newline),,$(subst ",\",$(1)))"
endef

define render_tmpl
	printf -- '$(subst $(newline),\n,$(subst %,%%,$(subst ','\'',$(subst $(tab),$(WS),$(call $(1))))))\n' > $(2)
endef

define bs_mix_Makefile
include elixir_deps.mk
include ../../erlang.mk
include ../elixir.mk/plugins.mk

compile-ex-dep: elixir-deps
\t@ MIX_ENV=$m $e/elixir $e/mix compile --no-deps-check
\t@ ln -s _build/$m/lib/$d/ebin ebin

endef

define elixir_deps.ex
	File.cd("$(1)");
	Application.ensure_all_started(:mix);
	Code.eval_file("mix.exs");
	Mix.Tasks.Local.Hex.run(["--force"]);
	Application.ensure_all_started(:hex);
	Hex.Utils.ensure_registry!();
	deps = Mix.Dep.loaded([]);
	deps = List.foldl(deps, [], fn(%Mix.Dep{app: name, opts: opts, requirement: _ver, scm: scm}, acc) ->
		only = Dict.get(opts, :only);
		if Mix.env == only or only == nil do
			case scm do
				Elixir.Hex.SCM -> 
				  version = Atom.to_string(name) |> Hex.Registry.get_versions |> List.last;
				  [{name, "hex #{Regex.replace(~r/[^\.0-9]/, version, "")}"}|acc];
				Elixir.Mix.SCM.Git -> [{name, "git #{Dict.get(opts, :git)} #{Dict.get(opts, :branch)}"}|acc];
				_ -> acc;
			end;
		else
			acc;
		end;
	end);
	{:ok, file} = File.open("elixir_deps.mk", [:write]);
	IO.write(file, "DEPS_DIR = $(2)\n");
	IO.write(file, "ELIXIR_MIX_ENV = $(3)\n");
	IO.write(file, "ELIXIR_DEPS = #{Enum.join(Dict.keys(deps), " ")}\n");
	Enum.each(deps, fn({name, dep}) ->
		IO.write(file, "dep_#{name} = #{dep}\n");
	end);
	File.close(file);

endef

define elixir_deps
	$(call elixir,$(call elixir_deps.ex,$(1),$(DEPS_DIR),$(ELIXIR_MIX_ENV)))
endef

define automix
	if [ -f $1/Makefile -o -f $1/rebar.config -o -f $1/rebar.config.script -o -f $1/configure.ac -o -f $1/configure.in -o -f $1/configure ]; then \
		$$(call dep_autopatch,$2); \
		$(MAKE) -C $1 IS_DEP=1; \
	else \
		$(call elixir_deps,$1); \
		$(call render_tmpl,bs_mix_Makefile,$1/Makefile.mix); \
		$(MAKE) -C $1 -f Makefile.mix compile-ex-dep; \
	fi
endef

define elixir_dep_target
$(eval t := $(DEPS_DIR)/$1)
$(eval d := $(DEPS_DIR)/$1/elixir_deps.mk)
$d: $(DEPS_DIR)/elixir
	$(eval DEP_NAME := $(call dep_name,$1))
	$(eval DEP_STR := $(if $(filter-out $1,$(DEP_NAME)),$1,"$1 ($(DEP_NAME))"))
	$(verbose) mkdir -p $(DEPS_DIR)
	$(dep_verbose) $(call dep_fetch_$(strip $(call dep_fetch,$(1))),$(1))
	$(eval e := $(ELIXIR_BIN))
	$(eval m := $(ELIXIR_MIX_ENV))
	$(eval d := $(1))
	$(verbose) $(call automix,$t,$1)
endef
$(foreach dep,$(ELIXIR_DEPS),$(eval $(call elixir_dep_target,$(dep))))

ifneq ($(SKIP_DEPS),)
elixir-deps:
else
elixir-deps: $(ALL_ELIXIR_DEPS_D)
endif

$(DEPS_DIR)/elixir:
	$(verbose) mkdir -p $(DEPS_DIR)
	$(call dep_verbose,elixir) $(call dep_fetch_$(strip $(call dep_fetch,elixir)),elixir)
	$(verbose) $(MAKE) -C $(DEPS_DIR)/elixir
	$(call dep_verbose,hex) 
	$(verbose) MIX_ENV=$(ELIXIR_MIX_ENV) $(ELIXIR_BIN)/elixir $(ELIXIR_BIN)/mix local.hex --force 2>&1 1>/dev/null

