import copy

import sane
from sane.helpers import recursive_update as dict_update
import wrf.custom_actions.run_wrf as run_wrf


@sane.register
def feature_restart_em_real( orch ):
  # cases - just hard-code since we need the path from the host
  cases = [
            "basic",              # failing in v4.8.0 @ e836cd6
            "dfi",
            "diff_opt_2",
            "km_opt_1",
            "km_opt_2",
            "km_opt_3",           # failing for some time
            "nest_starts_later",
            "nwp_diag",           # failing in v4.8.0 @ e836cd6
            "w_damping"           # failing in v4.8.0 @ e836cd6
            ]
  patches = {
              "km_opt_1" :
              {
                "dynamics" :
                {
                  "khdif" : [300,    300,    300],
                  "kvdif" : [ 3,      3,      3]
                }
              }
            }
  build = "build_make_em_real_gnu_debug_dmpar"
  for wrf_case in cases:
    init_wrf = run_wrf.InitWRF( f"restart_{wrf_case}_init" )
    restart  = run_wrf.RunWRFRestart( f"restart_{wrf_case}" )

    if wrf_case in patches:
      init_wrf.nml_patches = { "namelist.input.1" : patches[wrf_case] }
      restart.nml_patches  = {
                              "namelist.input.2" : patches[wrf_case],
                              "namelist.input.3" : patches[wrf_case]
                              }

    init_wrf.wrf_case        = wrf_case
    init_wrf.wrf_nml         = "namelist.input.1"
    init_wrf.wrf_case_path   = "${{ host_info.config.wrf_restart.run_wrf_case_path }}"
    init_wrf.wrf_met_path    = "${{ host_info.config.wrf_restart.run_wrf_met_path }}"
    init_wrf.wrf_met_folder  = "standard"
    init_wrf.wrf_dir         = "${{ dependencies.${{ config.build }}.outputs.build_dir }}/test/em_real"
    init_wrf.wrf_run_dir     = "regtests/output/restart_${{ wrf_case }}"
    init_wrf.environment     = "gnu"
    init_wrf.modify_environ  = True
    init_wrf.config["build"] = build
    init_wrf.extra_data.append( "${{ host_info.config.wrf_restart.run_wrf_met_path }}/additional" )

    init_wrf.add_resource_requirements( { "cpus" : 1, "timelimit" : "00:10:00" } )
    init_wrf.add_dependencies( build )

    # Inherit most attributes from init_wrf
    restart.config["build"] = build
    restart.environment     = "gnu"
    restart.wrf_nml         = "namelist.input.2"
    restart.wrf_restart_nml = "namelist.input.3"
    restart.wrf_diff_exec   = "${{ dependencies.${{ config.build }}.outputs.build_dir }}/external/io_netcdf/diffwrf"
    restart.add_dependencies( init_wrf.id, build )
    restart.add_resource_requirements( { "cpus" : 8, "timelimit" : "00:25:00" } )

    orch.add_action( init_wrf )
    orch.add_action( restart )
