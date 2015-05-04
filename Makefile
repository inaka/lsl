PROJECT = lsl

CONFIG ?= test/test.config

DEPS = eper mixer lager cowboy jiffy
SELL_DEPS = sync
TEST_DEPS = xref_runner shotgun

dep_cowboy = git git://github.com/extend/cowboy.git 1.0.1
dep_jiffy = git git://github.com/davisp/jiffy.git 0.13.3
dep_eper = git git://github.com/massemanet/eper.git 0.90.0
dep_mixer = git git://github.com/inaka/mixer.git 0.1.2
dep_sync = git git://github.com/inaka/sync.git 0.1
dep_shotgun = git git://github.com/inaka/shotgun.git 0.1.8
dep_xref_runner = git git://github.com/inaka/xref_runner.git 0.2.2

DIALYZER_DIRS := ebin/
DIALYZER_OPTS := --verbose --statistics -Werror_handling \
                 -Wrace_conditions #-Wunmatched_returns

include erlang.mk

COMPILE_FIRST += ai/lsl_ai

ERLC_OPTS += +'{parse_transform, lager_transform}'
ERLC_OPTS += +warn_unused_vars +warn_export_all +warn_shadow_vars +warn_unused_import +warn_unused_function
ERLC_OPTS += +warn_bif_clash +warn_unused_record +warn_deprecated_function +warn_obsolete_guard +strict_validation
ERLC_OPTS += +warn_export_vars +warn_exported_vars +warn_missing_spec +warn_untyped_record +debug_info

TEST_ERLC_OPTS += +'{parse_transform, lager_transform}'
CT_OPTS += -cover test/lsl.coverspec -vvv -erl_args -config ${CONFIG}

SHELL_OPTS += -name ${PROJECT}@`hostname` -config ${CONFIG} -s lager -s sync

erldocs:
	erldocs . -o docs
