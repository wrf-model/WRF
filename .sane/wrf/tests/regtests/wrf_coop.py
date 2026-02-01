import copy

import sane
from sane.helpers import recursive_update as dict_update
import wrf.custom_actions.run_wrf as run_wrf


@sane.register
def wrf_coop_reg_tests( orch ):
  # init and run are based off of this default
  default = {
              "wrf_run_dir"   : "regtests/output/${{ id }}",
              "environment" : "gnu",
              "modify_environ" : True,
            }
  # init settings, most of which are inherited to run
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
  # run settings
  default_run = {
                  "resources" : { "cpus" : 8 },
                }
  # override cummulative settings based on run mode
  default_par_opt = {
                     "serial" : { "resources" : { "timelimit" : "00:20:00", "cpus" : 1 } },
                     "openmp" : { "resources" : { "timelimit" : "00:20:00" } },
                     "mpi"    : { "resources" : { "timelimit" : "00:10:00" } },
                    }

  ##############################################################################
  ## Test cases
  ##
  ## These test cases are laid out to mirror the structure of the original
  ## WRF Coop testing layout. The root node is the primary test case that
  ## determines the parent namelist case folder. Under this dictionary
  ## a list of comparisons, target wrf/test/ folder and set of namelist to test
  ## are provided. The comparisons are used to determine run parameters as well
  ## as the sub-folder in the parent namelist case folder. The most notable
  ## customization here is that the "nml_cases" keys note the namelist.<suffix>
  ## while the dict value provides config overrides beyond the aggregated
  ## defaults. These dicts *can* also hold sub-dicts matching the "compare" list
  ## to provide comparison-specific overrides for that specific namelist.
  ##############################################################################
  wrf_cases = {
    # Most of these need BUILD_RRTMG_FAST=1 and BUILD_RRTMK=1
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
        "52DF" : { }
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

  ##############################################################################
  ## Create the Actions
  ##
  ## for each case:
  ##   for each the namelist that will be tested:
  ##     create an InitWRF action that will create the initial conditions for
  ##     this namelist only once
  ##
  ##     for each comparison type:
  ##       create a RunWRF with a dependency to the above InitWRF
  ##
  ##     if multiple comparisons:
  ##       create a comparison sane.Action dependent on all RunWRF for this nml
  ##
  ##     create a final sync sane.Action that does nothing but is dependent on
  ##     the comparison sane.Action or all the RunWRF if the comparison DNE
  ##############################################################################
  for wrf_case, case_dict in wrf_cases.items():
    # Loop over all the base and A-L cases
    build = f"build_make_{case_dict['target']}_gnu_debug_dm_sm"

    for nml_case, config in case_dict["nml_cases"].items():
      # Within a case, loop over the specific namelist to test
      # as well as any specific config options for that nml case
      nml = f"namelist.input.{nml_case}"
      base_opts = copy.deepcopy( default )
      base_opts["wrf_nml"]            = nml
      base_opts["config"]             = {}
      base_opts["config"]["case"]     = wrf_case
      base_opts["config"]["target"]   = case_dict["target"]
      base_opts["config"]["build"]    = build

      # Create the initial conditions for this nml
      init_wrf = run_wrf.InitWRF( f"{wrf_case}_{nml_case}_init" )
      init_wrf.omp_threads = 1
      init_wrf.use_omp     = True

      # Create the config dict to set the parameters for this init wrf
      opts = dict_update( copy.deepcopy( base_opts ), copy.deepcopy( default_init ) )
      opts = dict_update( opts, copy.deepcopy( default_par_opt["serial"] ) )
      opts["config"]["par_opt"] = "SERIAL"

      init_wrf.add_dependencies( opts["config"]["build"] )
      init_wrf.load_options( opts )
      orch.add_action( init_wrf )

      for comp in case_dict["compare"]:
        # For each nml case, run WRF multiple times decomposing the domain using
        # different methods and comparing at the end
        id = f"{wrf_case}_{nml_case}_{comp}"
        action = run_wrf.RunWRF( id )

        # Get the general options for this nml case
        general_opts = copy.deepcopy( config )
        # pull out the specifics
        specifics = { c : general_opts.pop( c, {} ) for c in case_dict["compare"] }

        # Create the config dict
        opts = dict_update( copy.deepcopy( base_opts ), copy.deepcopy( default_run ) )
        opts = dict_update( opts, copy.deepcopy( default_par_opt[comp]) )
        opts = dict_update( dict_update( opts, general_opts ), specifics[comp] )
        opts["config"]["par_opt"] = comp.upper()

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
        action.outputs["data"]      = opts["wrf_run_dir"]
        action.add_dependencies( opts["config"]["build"], init_wrf.id )
        action.load_options( opts )
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
