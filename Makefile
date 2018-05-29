PROJECT = ecf
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.0.1

DEPS = cowboy iso8601
dep_cowboy_commit = 2.4.0

LOCAL_DEPS = mnesia crypto

DEP_PLUGINS = cowboy

# workaround for dialyzer bug with PIC
#DIALYZER_OPTS = --no_native
DIALYZER_PLT_OPTS = --no_native

# Whitespace to be used when creating files from templates.
SP = 4

include erlang.mk

