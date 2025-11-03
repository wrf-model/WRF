import copy

import sane
from sane.json_config import recursive_update as dict_update
import wrf.custom_actions.run_wrf as run_wrf


@sane.register
def wrf_coop_reg_tests( orch ):
  default = {
              "environment" : "gnu",
              "modify_environ" : True,
              "resources" : { "cpus" : 8, "timelimit" : "00:10:00" },
              "wrf_run_dir"   : "regtests/output/${{ id }}",
              "wrf_case_path" : "${{ host_info.config.wrf_coop.run_wrf_case_path }}",
              "wrf_met_path"  : "${{ host_info.config.wrf_coop.run_wrf_met_path }}",
              "wrf_dir"       : "${{ dependencies.${{ config.build }}.outputs.build_dir }}/test/${{ config.wrf_dir }}",
              "wrf_case"      : "${{ config.case }}/${{ config.par_opt }}",
              "wrf_met_folder": "${{ config.case }}",
              "config" :
              {
                "wrf_dir" : "${{ config.case }}"
              }
            }
  wrf_cases = {
    "em_real" :
    {
      "compare" : ["serial", "mpi", "openmp"],
      "target"  : "em_real",
      "nml_cases" :
      {
        "3dtke"    : { },
        "rap"      : { },
        "conus"    : { },
        "tropical" : { }
      }
    },
    "em_realA" :
    {
      "compare" : ["serial", "mpi", "openmp"],
      "target"  : "em_real",
      "nml_cases" :
      {
        "03"   : { },
        "03DF" : { },
        "03FD" : { },
        "06"   : { },
        "07NE" : { "resources" : { "cpus" : 9 } }
      }
    }
  }
  par_opt = { "serial" : "serial", "openmp" : "smpar", "mpi" : "dmpar" }

  for wrf_case, case_dict in wrf_cases.items():
    for nml_case, config in case_dict["nml_cases"].items():
      nml = f"namelist.input.{nml_case}"
      for comp in case_dict["compare"]:
        id = f"{wrf_case}_{nml_case}_{comp}"
        action = run_wrf.RunWRF( id )
        cfg = dict_update( copy.deepcopy( default ), config )
        cfg["wrf_nml"] = nml

        cfg["config"]["case"]     = wrf_case
        cfg["config"]["par_opt"]  = comp.upper()
        cfg["config"]["build"]    = f"build_make_{case_dict['target']}_gnu_debug_{par_opt[comp]}"

        if comp == "mpi":
          action.use_mpi = True
          action.use_omp = False
        elif comp == "omp":
          action.use_mpi = False
          action.use_omp = True
        else:
          action.use_mpi = False
          action.use_omp = False

        action.outputs["data"] = cfg["wrf_run_dir"]
        action.add_dependencies( cfg["config"]["build"] )

        action.load_config( cfg )
        orch.add_action( action )
      if len( case_dict["compare"] ) > 1:
        # Create another action to compare all of them
        id = f"{wrf_case}_{nml_case}"
        action = sane.Action( id )
        action.config["command"] = ".workflow/wrf/scripts/compare_wrf.sh"
        action.config["arguments"] = [
                                      f"${{{{ dependencies.{wrf_case}_{nml_case}_{case_dict['compare'][0]}.config.build }}}}/external/io_netcdf/diffwrf",
                                      *[ f"${{{{ dependencies.{wrf_case}_{nml_case}_{comp}.outputs.data }}}}" for comp in case_dict["compare"] ]
                                      ]
        action.environment = default["environment"]
        # Not very intesive
        action.local = True
        action.add_dependencies( *[ f"{wrf_case}_{nml_case}_{comp}" for comp in case_dict["compare"] ] )
        orch.add_action( action )
    # Now add one final action that allows us to run this full case
    action = sane.Action( f"{wrf_case}" )
    action.config["command"] = "echo"
    action.config["arguments"] = [ "final sync step, nothing here" ]
    if len( case_dict["compare"] ) > 1:
      action.add_dependencies( *[ f"{wrf_case}_{nml_case}" for nml_case in case_dict["nml_cases"] ] )
    else:
      action.add_dependencies( *[ f"{wrf_case}_{nml_case}_{comp}" for comp in case_dict["compare"] for nml_case in case_dict["nml_cases"] ] )
    orch.add_action( action )





