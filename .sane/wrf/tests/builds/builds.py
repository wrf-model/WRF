import sane
import itertools

@sane.register
def add_build_for_envs_cmake( orch ):
  envs = [ "gnu", "intel-classic", "intel-oneapi", "nvhpc" ]
  env2opt = [ "gfortran", "ifort", "ifx", "pgf90" ]
  sm_opt = [ "ON", "OFF" ]
  dm_opt = [ "ON", "OFF" ]
  build_types = [ "Release", "Debug" ]
  configurations = { "ARW" : [ "EM_REAL", "EM_FIRE", "EM_B_WAVE" ] }
  orch.log( f"Creating CMake build permutations..." ) 
  for core, env, build_type, sm, dm in itertools.product( configurations, envs, build_types, sm_opt, dm_opt ):
    for case in configurations[core]:
      sm_desc = "_sm" if sm == "ON" else ""
      dm_desc = "_dm" if dm == "ON" else ""
      id = f"{case}_{env}_{build_type}{dm_desc}{sm_desc}".lower()

      action = sane.Action( f"build_cmake_{id}" )
      action.config["command"]     = ".sane/wrf/scripts/buildCMake.sh"
      action.config["compiler"]    = env2opt[envs.index(env)]
      action.config["build_dir"]   = "_${{ id }}"
      action.config["install_dir"] = orch.working_directory + "/install_${{ id }}"
      action.config["build_type"]  = build_type
      action.config["core"]        = core
      action.config["case"]        = case
      action.config["dm"]          = dm
      action.config["sm"]          = sm
      action.config["clean_build"] = True

      action.outputs["install_dir"] = action.config["install_dir"]
      action.outputs["build_dir"]   = action.config["build_dir"]
      action.outputs["diffwrf_nc"]  = action.config["install_dir"] + "/bin/diffwrf_nc"
      args = []

      config_cmd = [ "-p ${{ config.compiler }}", "-d", "${{ config.build_dir }}", "-i", "${{ config.install_dir }}" ]
      config_cmd.extend( [ "-x -- -DWRF_NESTING=BASIC", "-DCMAKE_BUILD_TYPE=${{ config.build_type }}" ] )
      config_cmd.extend( [ "-DWRF_CORE=${{ config.core }}", "-DWRF_CASE=${{ config.case }}" ] )
      config_cmd.extend( [ "-DUSE_MPI=${{ config.dm }}", "-DUSE_OPENMP=${{ config.sm }}" ] )
      build_cmd = [ "${{ config.build_dir }}", "-j ${{ resources.cpus }}" ]
      clean_cmd = [ "-d", "${{ config.build_dir }}", "-i", "${{ config.install_dir }}" ]

      args.extend( [ "-c", " ".join( config_cmd ) ] )
      args.extend( [ "-b", " ".join( build_cmd ) ] )
      args.extend( [ "-r", " ".join( clean_cmd ) ] )
      args.extend( [ "-f", "${{ config.clean_build }}" ] )

      action.config["arguments"] = args
      action.add_resource_requirements( { "cpus" : 8 } )
      action.environment = env
      orch.add_action( action )


@sane.register
def add_build_for_envs_make( orch ):
  envs = [ "gnu", "intel-classic", "intel-oneapi", "nvhpc" ]
  # Assumes x86
  env2opt = [ 32, 13, 76, 52 ]

  par_opt = { "serial" : 0, "sm" : 1, "dm" : 2, "dm_sm" : 3 }
  debug = [ True, False ]
  targets = [ "em_real", "em_fire", "em_b_wave" ]
  orch.log( f"Creating Make build permutations..." ) 
  for target, env, dbg, opt in itertools.product( targets, envs, debug, par_opt ):
    build_type = "debug" if dbg else "release"
    id = f"{target}_{env}_{build_type}_{opt}".lower()

    action = sane.Action( f"build_make_{id}" )
    action.config["command"]     = ".sane/wrf/scripts/buildMake.sh"
    action.config["compile_opt"] = env2opt[envs.index(env)] + par_opt[opt]
    action.config["install_dir"]   = "_${{ id }}"
    action.config["target"]      = target
    action.config["nesting"]     = 1
    action.config["par_opt"]     = opt
    action.config["optstr"]      = "-d" if dbg else ""
    action.config["clean_build"] = True
    
    action.outputs["install_dir"] = action.config["install_dir"]
    action.outputs["build_dir"]   = action.config["install_dir"]
    action.outputs["diffwrf_nc"]  = action.config["install_dir"] + "/external/io_netcdf/diffwrf"
    args = []

    args.extend( [ "-c", "${{ config.compile_opt }}", "-n", "${{ config.nesting }}" ] )
    if action.config["optstr"]:
      args.extend( [ "-o", "${{ config.optstr }}" ] )

    args.extend( [ "-b", "${{ config.target }} -j ${{ resources.cpus }}" ] )
    args.extend( [ "-d", "${{ config.install_dir }}" ] )
    args.extend( [ "-f", "${{ config.clean_build }}" ] )

    action.config["arguments"] = args
    action.add_resource_requirements( { "cpus" : 8 } )
    action.environment = env
    orch.add_action( action )


@sane.register
def add_chem_builds_make( orch ):
  configurations = { "em_chem" : "-d chem", "em_chem_kpp" : "-d chem kpp" }
  par_opt = { "serial" : 32, "mpi" : 34 }
  orch.log( "Creating chemistry Make build permutations..." )
  for case, mode in itertools.product( configurations, par_opt ):
    action = sane.Action( f"build_make_{case}_gnu_debug_{mode}" )
    action.config["command"]     = ".sane/wrf/scripts/buildMake.sh"
    action.config["compile_opt"] = par_opt[mode]
    action.config["build_dir"]   = orch.working_directory + "/_${{ id }}"
    action.config["install_dir"] = action.config["build_dir"]
    action.config["target"]      = "em_real"
    action.config["nesting"]     = 1
    action.config["par_opt"]     = mode
    action.config["optstr"]      = configurations[case]
    action.config["clean_build"] = True

    action.outputs["install_dir"] = action.config["install_dir"]
    action.outputs["build_dir"]   = action.config["build_dir"]
    action.outputs["diffwrf_nc"]  = action.config["install_dir"] + "/external/io_netcdf/diffwrf"
    args = []

    args.extend( [ "-c", "${{ config.compile_opt }}", "-n", "${{ config.nesting }}" ] )
    args.extend( [ "-o", "${{ config.optstr }}" ] )
    args.extend( [ "-b", "${{ config.target }} -j ${{ resources.cpus }}" ] )
    args.extend( [ "-d", "${{ config.install_dir }}" ] )
    args.extend( [ "-f", "${{ config.clean_build }}" ] )

    action.config["arguments"] = args
    action.add_resource_requirements( { "cpus" : 8, "memory" : "8gb", "timelimit" : "00:50:00" } )
    action.environment = "gnu"
    orch.add_action( action )


@sane.register
def add_chem_builds_cmake( orch ):
  configurations = { "em_chem" : "OFF", "em_chem_kpp" : "ON" }
  par_opt = { "serial" : "OFF", "mpi" : "ON" }
  orch.log( "Creating chemistry CMake build permutations..." )
  for case, mode in itertools.product( configurations, par_opt ):
    action = sane.Action( f"build_cmake_{case}_gnu_debug_{mode}" )
    action.config["command"]     = ".sane/wrf/scripts/buildCMake.sh"
    action.config["compiler"]    = "gfortran"
    action.config["build_dir"]   = orch.working_directory + "/_${{ id }}"
    action.config["install_dir"] = orch.working_directory + "/install_${{ id }}"
    action.config["build_type"]  = "Debug"
    action.config["core"]        = "ARW"
    action.config["case"]        = "EM_REAL"
    action.config["dm"]          = par_opt[mode]
    action.config["sm"]          = "OFF"
    action.config["chem"]        = "ON"
    action.config["kpp"]         = configurations[case]
    action.config["clean_build"] = True

    action.outputs["install_dir"] = action.config["install_dir"]
    action.outputs["build_dir"]   = action.config["build_dir"]
    action.outputs["diffwrf_nc"]  = action.config["install_dir"] + "/bin/diffwrf_nc"
    args = []

    config_cmd = [ "-p ${{ config.compiler }}", "-d", "${{ config.build_dir }}", "-i", "${{ config.install_dir }}" ]
    config_cmd.extend( [ "-x -- -DWRF_NESTING=BASIC", "-DCMAKE_BUILD_TYPE=${{ config.build_type }}" ] )
    config_cmd.extend( [ "-DWRF_CORE=${{ config.core }}", "-DWRF_CASE=${{ config.case }}" ] )
    config_cmd.extend( [ "-DUSE_OPENMP=${{ config.sm }}", "-DUSE_MPI=${{ config.dm }}" ] )
    config_cmd.extend( [ "-DENABLE_CHEM=${{ config.chem }}", "-DENABLE_KPP=${{ config.kpp }}" ] )
    build_cmd = [ "${{ config.build_dir }}", "-j ${{ resources.cpus }}" ]
    clean_cmd = [ "-d", "${{ config.build_dir }}", "-i", "${{ config.install_dir }}" ]

    args.extend( [ "-c", " ".join( config_cmd ) ] )
    args.extend( [ "-b", " ".join( build_cmd ) ] )
    args.extend( [ "-r", " ".join( clean_cmd ) ] )
    args.extend( [ "-f", "${{ config.clean_build }}" ] )

    action.config["arguments"] = args
    action.add_resource_requirements( { "cpus" : 8, "memory" : "8gb", "timelimit" : "00:50:00" } )
    action.environment = "gnu"
    orch.add_action( action )
