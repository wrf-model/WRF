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
      id = f"{core}_{case}_{env}_{build_type}{sm_desc}{dm_desc}".lower()

      action = sane.Action( f"build_cmake_{id}" )
      action.config["command"]     = ".sane/wrf/scripts/buildCMake.sh"
      action.config["compiler"]    = env2opt[envs.index(env)]
      action.config["build_dir"]   = "_${{ id }}"
      action.config["install_dir"] = "install_${{ id }}"
      action.config["build_type"]  = build_type
      action.config["core"]        = core
      action.config["case"]        = case
      action.config["dm"]          = dm
      action.config["sm"]          = sm

      action.outputs["install_dir"] = action.config["install_dir"]
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

      action.config["arguments"] = args
      action.add_resource_requirements( { "cpus" : 8 } )
      action.environment = env
      orch.add_action( action )


@sane.register
def add_build_for_envs_make( orch ):
  envs = [ "gnu", "intel-classic", "intel-oneapi", "nvhpc" ]
  # Assumes x86
  env2opt = [ 32, 13, 76, 52 ]

  par_opt = { "serial" : 0, "smpar" : 1, "dmpar" : 2, "dm_sm" : 3 }
  debug = [ True, False ]
  targets = [ "em_real", "em_fire", "em_b_wave" ]
  orch.log( f"Creating Make build permutations..." ) 
  for target, env, dbg, opt in itertools.product( targets, envs, debug, par_opt ):
    build_type = "debug" if dbg else "release"
    id = f"{target}_{env}_{build_type}_{opt}".lower()

    action = sane.Action( f"build_make_{id}" )
    action.config["command"]     = ".sane/wrf/scripts/buildMake.sh"
    action.config["compile_opt"] = env2opt[envs.index(env)] + par_opt[opt]
    action.config["build_dir"]   = "_${{ id }}"
    action.config["target"]      = target
    action.config["nesting"]     = 1
    action.config["par_opt"]     = opt
    action.config["optstr"]      = "-d" if dbg else ""
    action.outputs["build_dir"]  = action.config["build_dir"]
    args = []

    args.extend( [ "-c", "${{ config.compile_opt }}", "-n", "${{ config.nesting }}" ] )
    if action.config["optstr"]:
      args.extend( [ "-o", "${{ config.optstr }}" ] )

    args.extend( [ "-b", "${{ config.target }} -j ${{ resources.cpus }}" ] )
    args.extend( [ "-d", "${{ config.build_dir }}" ] )

    action.config["arguments"] = args
    action.add_resource_requirements( { "cpus" : 8 } )
    action.environment = env
    orch.add_action( action )


