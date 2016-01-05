PROJECT = lsl

CONFIG ?= test/test.config

DEPS = eper mixer lager cowboy jiffy sumo katana erlpass swagger trails sumo_rest
SELL_DEPS = sync
TEST_DEPS = xref_runner shotgun
BUILD_DEPS = inaka_mk hexer_mk

dep_lager       = git https://github.com/basho/lager.git           3.0.2
dep_erlpass     = git https://github.com/inaka/erlpass.git         06bcca3
dep_katana      = git https://github.com/inaka/erlang-katana.git   07efe94
dep_sumo        = git https://github.com/inaka/sumo_db.git         0.3.13
dep_cowboy      = git https://github.com/extend/cowboy.git         1.0.4
dep_jiffy       = git https://github.com/davisp/jiffy.git          0.14.4
dep_eper        = git https://github.com/massemanet/eper.git       0.96.4
dep_mixer       = git https://github.com/inaka/mixer.git           0.1.4
dep_sync        = git https://github.com/rustyio/sync.git          9c78e7b
dep_shotgun     = git https://github.com/inaka/shotgun.git         0.1.12
dep_sumo_rest   = git https://github.com/inaka/sumo_rest.git       a2b18f3
dep_swagger     = git https://github.com/inaka/cowboy-swagger.git  1f81947
dep_trails      = git https://github.com/inaka/cowboy-trails.git   0.1.0
dep_xref_runner = git https://github.com/inaka/xref_runner.git     0.2.2
dep_inaka_mk    = git https://github.com/inaka/inaka.mk.git        1.0.0
dep_hexer_mk    = git https://github.com/inaka/hexer.mk.git        1.0.0

LOCAL_DEPS := tools compiler syntax_tools common_test inets test_server dialyzer wx mnesia crypto

PLT_APPS := crypto mnesia

DEP_PLUGINS = inaka_mk hexer_mk

include erlang.mk

ERLC_OPTS := +'{parse_transform, lager_transform}' +'{lager_truncation_size, 65536}'
ERLC_OPTS += +warn_unused_vars +warn_export_all +warn_shadow_vars +warn_unused_import +warn_unused_function
ERLC_OPTS += +warn_bif_clash +warn_unused_record +warn_deprecated_function +warn_obsolete_guard +strict_validation
ERLC_OPTS += +warn_export_vars +warn_exported_vars +warn_missing_spec +warn_untyped_record +debug_info

TEST_ERLC_OPTS += +'{parse_transform, lager_transform}'
CT_OPTS += -cover test/${PROJECT}.coverspec -vvv -erl_args -config ${CONFIG}

SHELL_OPTS += -name ${PROJECT}@`hostname` -config ${CONFIG} -boot start_sasl -s lager -s sync -s ${PROJECT}

erldocs:
	erldocs . -o docs
