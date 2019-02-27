PROJECT = ecf
PROJECT_DESCRIPTION = Simple BEAM-powered forum software
PROJECT_VERSION = 0.0.1

BUILD_DEPS = parse_trans

DEPS = cowboy iso8601 jiffy erlydtl gen_smtp enacl
dep_cowboy_commit = 2.5.0
dep_enacl = git https://github.com/ecrownoffire/enacl xchacha

LOCAL_DEPS = mnesia crypto public_key ssl inets runtime_tools sasl

DEP_PLUGINS = cowboy

# Whitespace to be used when creating files from templates.
SP = 4

include erlang.mk

