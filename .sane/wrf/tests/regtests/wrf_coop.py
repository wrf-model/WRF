import copy

import sane
from sane.helpers import recursive_update as dict_update
import wrf.custom_actions.run_wrf as run_wrf


@sane.register
def wrf_coop_reg_tests( orch ):
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
        "3dtke"    : { "resources" : { "timelimit" : "00:45:00" } },
        "rap"      : { "resources" : { "timelimit" : "00:45:00" } },
        "conus"    : { "resources" : { "timelimit" : "00:45:00" } },
        "tropical" : { "resources" : { "timelimit" : "00:45:00" } }
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

  add_coop_cases( orch, wrf_cases )


@sane.register
def wrf_ideal_tests( orch ):
  # WRF Coop regression+feature build.csh TEST_GEN=SOME, matching em_real.
  # em_quarter_ss8 uses the em_quarter_ss executable compiled with real size 8.
  wrf_cases = {
    "em_quarter_ss" : {
      "target" : "em_quarter_ss",
      "compare" : [ "serial", "mpi", "openmp" ],
      "nml_cases" : { "02NE" : {}, "03" : {}, "03NE" : {} }
    },
    "em_quarter_ss8" : {
      "target" : "em_quarter_ss", "build_target" : "em_quarter_ss8",
      "compare" : [ "serial", "mpi", "openmp" ],
      "nml_cases" : { "08" : {}, "09" : {} }
    },
    "em_b_wave" : {
      "target" : "em_b_wave",
      "compare" : [ "serial", "mpi", "openmp" ],
      "nml_cases" : { "1NE" : {}, "2" : {}, "2NE" : {}, "3" : {} }
    },
    "em_fire" : {
      "target" : "em_fire",
      "compare" : [ "serial", "mpi", "openmp" ],
      "nml_cases" : { "01" : {} }
    },
    # WRF rejects distributed-memory builds for this 3-cell-wide 2D case.
    # Keep the true serial exception explicit here for both build systems.
    "em_hill2d_x" : {
      "target" : "em_hill2d_x",
      "builds" : {
        "make" : "build_make_em_hill2d_x_gnu_debug_serial",
        "cmake" : "build_cmake_em_hill2d_x_gnu_debug"
      },
      "options" : { "use_mpi" : False },
      "compare" : [ "serial" ],
      "nml_cases" : { "01" : {} }
    }
  }
  add_coop_cases( orch, wrf_cases, ideal=True )

  for case in [ *wrf_cases, "em_ideal" ]:
    suite = sane.Action( case )
    suite.environment = "gnu"
    suite.local = True
    suite.config["command"] = "echo"
    suite.config["arguments"] = [ f"{case}: Make and CMake ideal tests passed" ]
    dependencies = [ case ] if case != "em_ideal" else list( wrf_cases )
    suite.add_dependencies( *[ f"{dep}_{build}" for dep in dependencies for build in [ "make", "cmake" ] ] )
    suite.add_resource_requirements( { "cpus" : 1 } )
    orch.add_action( suite )


def add_coop_cases( orch, wrf_cases, ideal=False ):
  # init and run are based off of this default
  default = {
              "environment" : "gnu"
            }
  # init settings, most of which are inherited to run
  default_init = {
                  "wrf_nml"       : "${{ config.wrf_case_path }}/${{ config.case }}/${{ config.par_opt }}/${{ config.wrf_nml }}",
                  "wrf_met_path"  : "${{ host_info.config.wrf_coop.run_wrf_met_path }}/em_real",
                  "wrf_dir"       : "${{ dependencies.${{ config.build }}.outputs.install_dir }}/test/${{ config.wrf_dir }}",
                  "resources"     : { "cpus" : 1 },
                  "config" :
                  {
                    "wrf_case_path" : "${{ host_info.config.wrf_coop.run_wrf_case_path }}"
                    # To be filled out by the action creation
                    # "wrf_dir"  : "",
                    # "wrf_nml"  : "",
                    # "wrf_case" : ""
                  }
                 }
  # run settings
  default_run = {
                  "resources" : { "cpus" : 8 },
                }
  # override cummulative settings based on run mode
  default_par_opt = {
                     "serial" : { "resources" : { "timelimit" : "00:25:00", "cpus" : 1 } },
                     "openmp" : { "resources" : { "timelimit" : "00:25:00" } },
                     "mpi"    : { "resources" : { "timelimit" : "00:15:00" } },
                    }

  if ideal:
    default_init["wrf_nml"] = "${{ config.wrf_case_path }}/${{ config.case }}/${{ config.wrf_nml }}"
    default_init["wrf_met_path"] = "${{ host_info.config.wrf_coop.run_wrf_met_path }}/${{ config.case }}"
    default_init["wrf_exec"] = "ideal.exe"

  builds = [
            ( "build_make_{core}_gnu_debug_dm_sm", "make" ),
            ( "build_cmake_{core}_gnu_debug_dm_sm", "cmake" )
            ]

  ##############################################################################
  ## Create the Actions
  ##
  ## for each build:
  ##   for each case:
  ##     for each the namelist that will be tested:
  ##       create an InitWRF action that will create the initial conditions for
  ##       this namelist only once
  ##  
  ##       for each comparison type:
  ##         create a RunWRF with a dependency to the above InitWRF
  ##  
  ##       if multiple comparisons:
  ##         create a comparison sane.Action dependent on all RunWRF for this nml
  ##  
  ##       create a final sync sane.Action that does nothing but is dependent on
  ##       the comparison sane.Action or all the RunWRF if the comparison DNE
  ##############################################################################
  for build_name, build_type in builds:
    for wrf_case, case_dict in wrf_cases.items():
      # Loop over the selected WRF Coop cases
      build = case_dict.get( "builds", {} ).get(
        build_type, build_name.format( core=case_dict.get( 'build_target', case_dict['target'] ) )
      )

      for nml_case, config in case_dict["nml_cases"].items():
        # Within a case, loop over the specific namelist to test
        # as well as any specific config options for that nml case
        nml = f"namelist.input.{nml_case}"
        base_opts = dict_update( copy.deepcopy( default ), copy.deepcopy( case_dict.get( "options", {} ) ) )
        base_opts["wrf_run_dir"]        = "regtests/output/${{ id }}"

        # We will use nested config options to facilitate easy namelist selection
        # that routes to the case folder style setup
        base_opts["config"]             = {}
        base_opts["config"]["case"]     = wrf_case
        base_opts["config"]["wrf_nml"]  = nml
        base_opts["config"]["wrf_dir"]  = case_dict["target"]
        base_opts["config"]["build"]    = build
        base_opts["modify_environ"]     = build_type == "make"

        # Create the initial conditions for this nml
        init_wrf = run_wrf.InitWRF( f"{wrf_case}_{nml_case}_init_{build_type}" )
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
          id = f"{wrf_case}_{nml_case}_{comp}_{build_type}"
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

          # Select pseudo-serial operation for parallel builds
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
          id = f"{wrf_case}_{nml_case}_{build_type}"
          action = sane.Action( id )
          action.config["command"] = ".sane/wrf/scripts/compare_wrf.sh"
          action.config["arguments"] = [
                                        "${{ dependencies.${{ config.build }}.outputs.diffwrf_nc }}",
                                        *[ f"${{{{ dependencies.{wrf_case}_{nml_case}_{comp}_{build_type}.outputs.data }}}}" for comp in case_dict["compare"] ]
                                        ]
          action.config["build"] = build
          action.environment = default["environment"]
          # Not very intesive
          action.local = True
          action.add_dependencies( *[ f"{wrf_case}_{nml_case}_{comp}_{build_type}" for comp in case_dict["compare"] ], build )
          action.add_resource_requirements( { "cpus" : 1 } )
          orch.add_action( action )
      # Now add one final action that allows us to run this full case
      action = sane.Action( f"{wrf_case}_{build_type}" )
      action.environment = default["environment"]
      action.add_resource_requirements( { "cpus" : 1 } )
      # just a sync
      action.local = True
      action.config["command"] = "echo"
      action.config["arguments"] = [ "final sync step, nothing here" ]
      if len( case_dict["compare"] ) > 1:
        action.add_dependencies( *[ f"{wrf_case}_{nml_case}_{build_type}" for nml_case in case_dict["nml_cases"] ] )
      else:
        action.add_dependencies( *[ f"{wrf_case}_{nml_case}_{comp}_{build_type}" for comp in case_dict["compare"] for nml_case in case_dict["nml_cases"] ] )
      orch.add_action( action )


@sane.register
def wrf_chem_tests( orch ):
  cases = { "em_chem" : [ "1", "2", "5" ], "em_chem_kpp" : [ "120" ] }
  for build_type in [ "make", "cmake" ]:
    for case, namelists in cases.items():
      builds = {
        mode : f"build_{build_type}_{case}_gnu_debug_{mode}"
        for mode in [ "serial", "mpi" ]
      }

      comparisons = []
      for nml in namelists:
        runs = []
        for mode in builds:
          init = run_wrf.InitWRF( f"{case}_{nml}_init_{mode}_{build_type}" )
          init.environment = "gnu"
          # init.wrf_case = case
          init.wrf_nml = "${{ config.wrf_case_path }}/${{ config.wrf_case }}/${{ config.wrf_nml }}"
          init.wrf_met_path = "${{ host_info.config.wrf_coop.run_wrf_met_path }}/${{ config.wrf_case }}/"
          init.wrf_dir = f"${{{{ dependencies.{builds[mode]}.outputs.install_dir }}}}/test/em_real"

          init.config["wrf_nml"]  = f"namelist.input.{nml}"
          init.config["wrf_case"] = case
          init.config["wrf_case_path"] = "${{ host_info.config.wrf_coop.run_wrf_case_path }}"

          init.wrf_run_dir = "regtests/output/${{ id }}"
          init.modify_environ = build_type == "make"
          init.use_mpi = mode == "mpi"
          init.add_dependencies( builds[mode] )
          resources = { "cpus" : 8 if mode == "mpi" else 1,
                        "memory" : "4gb", "timelimit" : "00:30:00" }
          init.add_resource_requirements( resources )
          orch.add_action( init )

          run = run_wrf.RunWRF( f"{case}_{nml}_{mode}_{build_type}" )
          run.environment = "gnu"
          run.use_mpi = mode == "mpi"
          # Inherit the init directory, including metfiles and chemistry inputs.
          run.add_dependencies( init.id )
          run.add_resource_requirements( resources )
          orch.add_action( run )
          runs.append( run.id )

        comparison = sane.Action( f"{case}_{nml}_{build_type}" )
        comparison.environment = "gnu"
        comparison.local = True
        comparison.config["command"] = ".sane/wrf/scripts/compare_wrf.sh"
        comparison.config["arguments"] = [
          f"${{{{ dependencies.{builds['serial']}.outputs.diffwrf_nc }}}}",
          *[ f"${{{{ dependencies.{run}.outputs.wrf_run_dir }}}}" for run in runs ]
        ]
        comparison.add_dependencies( builds["serial"], *runs )
        comparison.add_resource_requirements( { "cpus" : 1 } )
        orch.add_action( comparison )
        comparisons.append( comparison.id )

      suite = sane.Action( f"{case}_{build_type}" )
      suite.environment = "gnu"
      suite.local = True
      suite.config["command"] = "echo"
      suite.config["arguments"] = [ f"{case} ({build_type}): all serial/MPI comparisons passed" ]
      suite.add_dependencies( *comparisons )
      suite.add_resource_requirements( { "cpus" : 1 } )
      orch.add_action( suite )

  for case in cases:
    suite = sane.Action( case )
    suite.environment = "gnu"
    suite.local = True
    suite.config["command"] = "echo"
    suite.config["arguments"] = [ f"{case}: Make and CMake comparisons passed" ]
    suite.add_dependencies( f"{case}_make", f"{case}_cmake" )
    suite.add_resource_requirements( { "cpus" : 1 } )
    orch.add_action( suite )
