import copy

import sane
from sane.json_config import recursive_update as dict_update
import wrf.custom_actions.run_wrf as run_wrf


@sane.register
def wrf_coop_reg_tests( orch ):
  default = {
              "wrf_run_dir"   : "regtests/output/${{ id }}",
              "environment" : "gnu",
              "modify_environ" : True,
            }
  default_init = {
                  "wrf_case"      : "${{ config.case }}/${{ config.par_opt }}",
                  "wrf_case_path" : "${{ host_info.config.wrf_coop.run_wrf_case_path }}",
                  "wrf_met_path"  : "${{ host_info.config.wrf_coop.run_wrf_met_path }}",
                  "wrf_met_folder": "em_real",
                  "wrf_dir"       : "${{ dependencies.${{ config.build }}.outputs.build_dir }}/test/${{ config.wrf_dir }}",
                  "resources"     : { "cpus" : 1 },
                  "config" :
                  {
                    "wrf_dir" : "${{ config.target }}"
                  }
                 }
  default_run = {
                  "resources" : { "cpus" : 8 },
                }
  default_par_opt = {
                     "serial" : { "resources" : { "timelimit" : "00:20:00", "cpus" : 1 } },
                     "openmp" : { "resources" : { "timelimit" : "00:20:00" } },
                     "mpi"    : { "resources" : { "timelimit" : "00:10:00" } },
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
        "03"   : { },  # needs BUILD_RRTMG_FAST and BUILD_RRTMK
        "03DF" : { }
      }
    },
    "em_realB" :
    {
      "compare" : ["serial", "mpi", "openmp"],
      "target"  : "em_real",
      "nml_cases" :
      {
        "10" : { },
        "11" : { },
        "14" : { },
        "16" : { }
      }
    },
    "em_realC" :
    {
      "compare" : ["serial", "mpi", "openmp"],
      "target"  : "em_real",
      "nml_cases" :
      {
        "17"   : { },
        "18"   : { },
        "20"   : { }
      }
    },
    "em_realD" :
    {
      "compare" : ["serial", "mpi", "openmp"],
      "target"  : "em_real",
      "nml_cases" :
      {
        "38"   : { },
        "48"   : { },
        "49"   : { }
      }
    },
    "em_realE" :
    {
      "compare" : ["serial", "mpi", "openmp"],
      "target"  : "em_real",
      "nml_cases" :
      {
        "52DF" : { }  # needs BUILD_RRTMG_FAST and BUILD_RRTMK
      }
    },
    "em_realF" :
    {
      "compare" : ["serial", "mpi", "openmp"],
      "target"  : "em_real",
      "nml_cases" :
      {
        "65DF" : { }
      }
    },
    "em_realG" :
    {
      "compare" : ["serial", "mpi", "openmp"],
      "target"  : "em_real",
      "nml_cases" :
      {
        "kiaps1NE" : { "resources" : { "cpus" : 9 } },
        "kiaps2" : { }
      }
    },
    "em_realH" :
    {
      "compare" : ["serial", "mpi" ],
      "target"  : "em_real",
      "nml_cases" :
      {
        "52"       : { },
        "cmt"      : { },
        "solaraNE" :  { "resources" : { "cpus" : 9 } },
        "urb3bNE"  :  { "resources" : { "cpus" : 9 } },
        "03ST"     : { },
      }
    },
    "em_realI" :
    {
      "compare" : ["serial", "mpi", "openmp"],
      "target"  : "em_real",
      "nml_cases" :
      {
        "03FD" : { },
        "06"   : { }
      }
    },
    "em_realJ" :
    {
      "compare" : ["serial", "mpi", "openmp"],
      "target"  : "em_real",
      "nml_cases" :
      {
        "50" : { },
        "51" : { }
      }
    },
    "em_realK" :
    {
      "compare" : ["serial", "mpi", "openmp"],
      "target"  : "em_real",
      "nml_cases" :
      {
        "52FD" : { },
        "60"   : { },
        "60NE" : { "resources" : { "cpus" : 9 } }
      }
    },
    "em_realL" :
    {
      "compare" : ["serial", "mpi", "openmp"],
      "target"  : "em_real",
      "nml_cases" :
      {
        "66FD" : { },
        "71"   : { },
        "78"   : { },
        "79"   : { }
      }
    }
  }

  for wrf_case, case_dict in wrf_cases.items():
    # Loop over all the base and A-L cases
    build = f"build_make_{case_dict['target']}_gnu_debug_dm_sm"

    for nml_case, config in case_dict["nml_cases"].items():
      # Within a case, loop over the specific namelist to test
      # as well as any specific config options for that nml case
      nml = f"namelist.input.{nml_case}"
      base_cfg = copy.deepcopy( default )
      base_cfg["wrf_nml"]            = nml
      base_cfg["config"]             = {}
      base_cfg["config"]["case"]     = wrf_case
      base_cfg["config"]["target"]   = case_dict["target"]
      base_cfg["config"]["build"]    = build

      # Create the initial conditions for this nml
      init_wrf = run_wrf.InitWRF( f"{wrf_case}_{nml_case}_init" )
      init_wrf.omp_threads = 1
      init_wrf.use_omp     = True

      # Create the config dict to set the parameters for this init wrf
      cfg = dict_update( copy.deepcopy( base_cfg ), copy.deepcopy( default_init ) )
      cfg = dict_update( cfg, copy.deepcopy( default_par_opt["serial"] ) )
      cfg["config"]["par_opt"] = "SERIAL"

      init_wrf.add_dependencies( cfg["config"]["build"] )
      init_wrf.load_config( cfg )
      orch.add_action( init_wrf )

      for comp in case_dict["compare"]:
        # For each nml case, run WRF multiple times decomposing the domain using
        # different methods and comparing at the end
        id = f"{wrf_case}_{nml_case}_{comp}"
        action = run_wrf.RunWRF( id )

        # Get the general options for this nml case
        general_cfg = copy.deepcopy( config )
        # pull out the specifics
        specifics = { c : general_cfg.pop( c, {} ) for c in case_dict["compare"] }

        # Create the config dict
        cfg = dict_update( copy.deepcopy( base_cfg ), copy.deepcopy( default_run ) )
        cfg = dict_update( cfg, copy.deepcopy( default_par_opt[comp]) )
        cfg = dict_update( dict_update( cfg, general_cfg ), specifics[comp] )
        cfg["config"]["par_opt"] = comp.upper()

        # Force psuedo-serial operation
        action.use_mpi = True
        action.use_omp = True
        if comp == "mpi":
          action.omp_threads = 1
        elif comp == "openmp":
          action.mpi_ranks   = 1
        else:
          action.mpi_ranks   = 1
          action.omp_threads = 1

        # Capture output data
        action.outputs["data"]      = cfg["wrf_run_dir"]
        action.add_dependencies( cfg["config"]["build"], init_wrf.id )
        action.load_config( cfg )
        orch.add_action( action )

      if len( case_dict["compare"] ) > 1:
        # Create another action to compare all of them
        id = f"{wrf_case}_{nml_case}"
        action = sane.Action( id )
        action.config["command"] = ".sane/wrf/scripts/compare_wrf.sh"
        action.config["arguments"] = [
                                      "${{ dependencies.${{ config.build }}.outputs.build_dir }}/external/io_netcdf/diffwrf",
                                      *[ f"${{{{ dependencies.{wrf_case}_{nml_case}_{comp}.outputs.data }}}}" for comp in case_dict["compare"] ]
                                      ]
        action.config["build"] = build
        action.environment = default["environment"]
        # Not very intesive
        action.local = True
        action.add_dependencies( *[ f"{wrf_case}_{nml_case}_{comp}" for comp in case_dict["compare"] ], build )
        action.add_resource_requirements( { "cpus" : 1 } )
        orch.add_action( action )
    # Now add one final action that allows us to run this full case
    action = sane.Action( f"{wrf_case}" )
    # just a sync
    action.local = True
    action.config["command"] = "echo"
    action.config["arguments"] = [ "final sync step, nothing here" ]
    if len( case_dict["compare"] ) > 1:
      action.add_dependencies( *[ f"{wrf_case}_{nml_case}" for nml_case in case_dict["nml_cases"] ] )
    else:
      action.add_dependencies( *[ f"{wrf_case}_{nml_case}_{comp}" for comp in case_dict["compare"] for nml_case in case_dict["nml_cases"] ] )
    orch.add_action( action )





