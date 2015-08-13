PROJECT = lsl

CONFIG ?= test/test.config

DEPS = eper mixer lager cowboy jiffy sumo katana bcrypt erlpass
SELL_DEPS = sync
TEST_DEPS = xref_runner shotgun

dep_lager = git https://github.com/basho/lager.git 3.0.1
dep_bcrypt = git https://github.com/opscode/erlang-bcrypt.git 0.5.0.3
dep_erlpass = git https://github.com/inaka/erlpass.git 1.0.2
dep_katana = git https://github.com/inaka/erlang-katana.git 0.2.8
dep_sumo = git https://github.com/inaka/sumo_db.git 0.3.11
dep_cowboy = git https://github.com/extend/cowboy.git 1.0.2
dep_jiffy = git https://github.com/davisp/jiffy.git 446e284b
dep_eper = git https://github.com/massemanet/eper.git 0.92.0
dep_mixer = git https://github.com/inaka/mixer.git 0.1.3
dep_sync = git https://github.com/inaka/sync.git 0.1.3
dep_shotgun = git https://github.com/inaka/shotgun.git 0.1.12
dep_xref_runner = git https://github.com/inaka/xref_runner.git 0.2.2

PLT_APPS := crypto mnesia
DIALYZER_DIRS := ebin/
DIALYZER_OPTS := --verbose --statistics -Werror_handling \
                 -Wrace_conditions #-Wunmatched_returns

include erlang.mk

COMPILE_FIRST += ai/lsl_ai

ERLC_OPTS := +'{parse_transform, lager_transform}' +'{lager_truncation_size, 65536}'
ERLC_OPTS += +warn_unused_vars +warn_export_all +warn_shadow_vars +warn_unused_import +warn_unused_function
ERLC_OPTS += +warn_bif_clash +warn_unused_record +warn_deprecated_function +warn_obsolete_guard +strict_validation
ERLC_OPTS += +warn_export_vars +warn_exported_vars +warn_missing_spec +warn_untyped_record +debug_info

TEST_ERLC_OPTS += +'{parse_transform, lager_transform}'
CT_OPTS += -cover test/${PROJECT}.coverspec -vvv -erl_args -config ${CONFIG}

SHELL_OPTS += -name ${PROJECT}@`hostname` -config ${CONFIG} -boot start_sasl -s lager -s sync -s ${PROJECT}

quicktests: app build-ct-suites
	@if [ -d "test" ] ; \
	then \
		mkdir -p logs/ ; \
		$(CT_RUN) -suite $(addsuffix _SUITE,$(CT_SUITES)) $(CT_OPTS) ; \
	fi
	$(gen_verbose) rm -f test/*.beam

erldocs:
	erldocs . -o docs
