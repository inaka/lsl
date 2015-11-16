PROJECT = lsl

CONFIG ?= test/test.config

DEPS = eper mixer lager cowboy jiffy sumo katana bcrypt erlpass
SELL_DEPS = sync
TEST_DEPS = xref_runner shotgun

dep_lager       = git https://github.com/basho/lager.git           3.0.2
dep_bcrypt      = git https://github.com/opscode/erlang-bcrypt.git 0.5.0.3
dep_erlpass     = git https://github.com/inaka/erlpass.git         1.0.3
dep_katana      = git https://github.com/inaka/erlang-katana.git   0.2.14
dep_sumo        = git https://github.com/inaka/sumo_db.git         0.3.13
dep_cowboy      = git https://github.com/extend/cowboy.git         1.0.3
dep_jiffy       = git https://github.com/davisp/jiffy.git          0.14.3
dep_eper        = git https://github.com/massemanet/eper.git       0.96.1
dep_mixer       = git https://github.com/inaka/mixer.git           0.1.4
dep_sync        = git https://github.com/inaka/sync.git            0.1.3
dep_shotgun     = git https://github.com/inaka/shotgun.git         0.1.12
dep_xref_runner = git https://github.com/inaka/xref_runner.git     0.2.2

LOCAL_DEPS := tools compiler syntax_tools common_test inets test_server dialyzer wx mnesia crypto

# xmerl tools compiler syntax_tools common_test inets ssl public_key test_server dialyzer wx

PLT_APPS := crypto mnesia
DIALYZER_DIRS := ebin/
DIALYZER_OPTS := --verbose --statistics -Werror_handling \
                 -Wrace_conditions #-Wunmatched_returns

include erlang.mk

DIALYZER_DIRS := ebin/ test/
DIALYZER_OPTS += --verbose --statistics -Wunmatched_returns

ERLC_OPTS := +'{parse_transform, lager_transform}' +'{lager_truncation_size, 65536}'
ERLC_OPTS += +warn_unused_vars +warn_export_all +warn_shadow_vars +warn_unused_import +warn_unused_function
ERLC_OPTS += +warn_bif_clash +warn_unused_record +warn_deprecated_function +warn_obsolete_guard +strict_validation
ERLC_OPTS += +warn_export_vars +warn_exported_vars +warn_missing_spec +warn_untyped_record +debug_info

TEST_ERLC_OPTS += +'{parse_transform, lager_transform}'
CT_OPTS += -cover test/${PROJECT}.coverspec -vvv -erl_args -config ${CONFIG}

SHELL_OPTS += -name ${PROJECT}@`hostname` -config ${CONFIG} -boot start_sasl -s lager -s sync -s ${PROJECT}

quicktests: app
	@$(MAKE) --no-print-directory app-build test-dir ERLC_OPTS="$(TEST_ERLC_OPTS)"
	$(verbose) mkdir -p $(CURDIR)/logs/
	$(gen_verbose) $(CT_RUN) -suite $(addsuffix _SUITE,$(CT_SUITES)) $(CT_OPTS)

test-build-plt: ERLC_OPTS=$(TEST_ERLC_OPTS)
test-build-plt:
	@$(MAKE) --no-print-directory test-dir ERLC_OPTS="$(TEST_ERLC_OPTS)"
	$(gen_verbose) touch ebin/test

plt-all: PLT_APPS := $(ALL_TEST_DEPS_DIRS)
plt-all: test-deps test-build-plt plt

dialyze-all: app test-build-plt dialyze

erldocs:
	erldocs . -o docs
