PROJECT = ecf
PROJECT_DESCRIPTION = Simple BEAM-powered forum software
PROJECT_VERSION = 0.0.1

BUILD_DEPS = parse_trans

DEPS = cowboy iso8601 jiffy erlydtl gen_smtp enacl
dep_cowboy_commit = 2.5.0
dep_enacl = git https://github.com/ecrownoffire/enacl

LOCAL_DEPS = mnesia crypto public_key ssl inets runtime_tools sasl

DEP_PLUGINS = cowboy

# workaround for dialyzer bug with PIC
# also handle parse transforms for dialyzer (erlang.mk #814)
DIALYZER_OPTS = --no_native -pa ebin -Werror_handling -Wrace_conditions -Wunmatched_returns
DIALYZER_PLT_OPTS = --no_native

# Whitespace to be used when creating files from templates.
SP = 4

include erlang.mk

