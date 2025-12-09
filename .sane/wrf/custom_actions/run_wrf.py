import os
import itertools
import shutil

import sane


class WRFBase( sane.Action ):
  def __init__( self, id ):
    super().__init__( id )
    self.wrf_case       = None
    self.wrf_case_path  = "${{ host_info.config.run_wrf_case_path }}"
    self.wrf_dir        = "test/em_real"
    self.wrf_run_dir    = "./output/${{ wrf_case }}"

    self.modify_environ = False

    self.wrf_exec       = None
    self.wrf_nml        = "namelist.input"

    # Control execution
    self.mpi_cmd        = "mpirun -np ${{ mpi_ranks }}"
    self.use_mpi        = True
    self.use_omp        = False
    self.mpi_ranks      = "${{ resources.cpus }}"
    self.omp_threads    = "${{ resources.cpus }}"

    # Make sure we can pass on all this info
    self.outputs["wrf_case"]       = "${{ wrf_case }}"
    self.outputs["wrf_case_path"]  = "${{ wrf_case_path }}"
    self.outputs["wrf_dir"]        = "${{ wrf_dir }}"
    self.outputs["wrf_run_dir"]    = "${{ wrf_run_dir }}"
    self.outputs["modify_environ"] = "${{ modify_environ }}"
    self.outputs["wrf_exec"]       = "${{ wrf_exec }}"
    self.outputs["wrf_nml"]        = "${{ wrf_nml }}"
    self.outputs["mpi_cmd"]        = "${{ mpi_cmd }}"
    self.outputs["use_mpi"]        = "${{ use_mpi }}"
    self.outputs["use_omp"]        = "${{ use_omp }}"
    self.outputs["mpi_ranks"]      = "${{ mpi_ranks }}"
    self.outputs["omp_threads"]    = "${{ omp_threads }}"

  def load_extra_config( self, config, origin ):
    self.wrf_case       = config.pop( "wrf_case", None )
    self.wrf_case_path  = config.pop( "wrf_case_path", self.wrf_case_path )
    self.wrf_run_dir    = config.pop( "wrf_run_dir", self.wrf_run_dir )
    self.wrf_dir        = config.pop( "wrf_dir", self.wrf_dir )

    # Do not check for execs existing yet as those may be created by other actions
    self.wrf_exec       = config.pop( "wrf_exec", self.wrf_exec )
    self.wrf_nml        = config.pop( "wrf_nml", self.wrf_nml )

    self.mpi_cmd        = config.pop( "mpi_cmd",      self.mpi_cmd )
    self.use_mpi        = config.pop( "use_mpi",      self.use_mpi )
    self.use_omp        = config.pop( "use_omp",      self.use_omp )
    self.mpi_ranks      = config.pop( "mpi_ranks",    self.mpi_ranks )
    self.omp_threads    = config.pop( "omp_threads",  self.omp_threads )

    self.modify_environ    = config.pop( "modify_environ",  self.modify_environ )
    super().load_extra_config( config, origin )

  def pre_launch( self ):
    # Preflight checks
    self.push_logscope( "pre_launch" )

    # case path and case exist, force assignment check
    self.wrf_case_path  = self.resolve_path_exists( self.dereference( self.wrf_case_path ) )
    self.wrf_case       = self.dereference( self.wrf_case )

    if self.wrf_case is None:
      msg = "No case provided"
      self.log( msg, level=50 )
      raise ValueError( msg )
    # case nml exist
    full_case_path = self.resolve_path_exists( os.path.join( self.wrf_case_path, self.wrf_case ) )
    self.wrf_nml       = self.dereference( self.wrf_nml )
    self.file_exists_in_path( full_case_path, self.wrf_nml )

    self.wrf_nml = self.dereference( self.wrf_nml )

    if self.use_mpi:
      self.log( f"Adding MPI command to arguments for wrf  : '{self.mpi_cmd}'" )
      self.config["arguments"].extend( [ "-p", self.mpi_cmd ] )
    if self.use_omp:
      self.log( f"Adding OMPTHREADS count to arguments : '{self.omp_threads}'" )
      self.config["arguments"].extend( [ "-o", self.omp_threads ] )

    if self.use_mpi and self.use_omp:
      if self.mpi_ranks == self.omp_threads and self.mpi_ranks == "${{ resources.cpus }}":
        msg = "Directly set the MPI ranks and ompthreads for this action instead of default '${{ resources.cpus }}'"
        self.log( msg, level=40 )
        raise Exception( msg )

    self.pop_logscope()

  def pre_run( self ):
    # Now check for things that should be here for sure since any dependencies would be 
    # finished by now
    self.push_logscope( "pre_run" )
    full_case_path = self.resolve_path_exists( os.path.join( self.wrf_case_path, self.wrf_case ) )

    # build location exists
    self.wrf_dir = self.dereference( self.wrf_dir )
    self.wrf_dir = self.resolve_path_exists( self.wrf_dir )

    # execs exist
    self.file_exists_in_path( self.wrf_dir, self.wrf_exec )

    if self.modify_environ:
      self.log( "Adding to LD_LIBRARY_PATH..." )
      ld_lib  = os.environ.get( "LD_LIBRARY_PATH", "" )
      ld_lib += f':{os.environ["NETCDF"]}/lib:{os.environ["NETCDF"]}/lib64'
      os.environ["LD_LIBRARY_PATH"] = ld_lib

    self.pop_logscope()

  def setup_dir( self ):
    # OK! Create run dir
    self.wrf_run_dir = self.dereference( self.wrf_run_dir )
    self.wrf_run_dir = self.resolve_path( self.working_directory, self.wrf_run_dir )
    if os.path.isdir( self.wrf_run_dir ):
      self.log( f"Cleaning '{self.wrf_run_dir}'" )
      shutil.rmtree( self.wrf_run_dir )
    os.makedirs( self.wrf_run_dir, exist_ok=True )

  def setup_wrf( self ):
    full_case_path = self.resolve_path_exists( os.path.join( self.wrf_case_path, self.wrf_case ) )

    # copy over execs, then metfiles, then case to run dir
    prev_exec_raw = self.__exec_raw__
    self.__exec_raw__ = False
    # This should work as everything should be absolute paths
    self.log( "Linking WRF executables..." )
    self.execute_subprocess( "ln", [ "-svf", os.path.join( self.wrf_dir, "*" ), self.wrf_run_dir ], verbose=True, shell=True )

    self.log( "Copying WRF case files..." )
    self.execute_subprocess( "cp", [ "-v", "--remove-destination", os.path.join( full_case_path, "*" ), self.wrf_run_dir ], verbose=True, shell=True )
    self.__exec_raw__ = prev_exec_raw


class InitWRF( WRFBase ):
  def __init__( self, id ):
    super().__init__( id )
    self.wrf_exec       = "real.exe"

    self.wrf_met_folder = "${{ wrf_case }}"
    self.wrf_met_path   = "${{ host_info.config.run_wrf_met_path }}"

    self.outputs["wrf_met_folder"] = "${{ wrf_met_folder }}"
    self.outputs["wrf_met_path"]   = "${{ wrf_met_path }}"

    # Not user input
    self.config["command"] = ".sane/wrf/scripts/run_init.sh"
    self.config["arguments"] = [
                                "-f", "${{ wrf_run_dir }}",
                                "-r", "${{ wrf_exec }}",
                                "-n", "${{ wrf_nml }}"
                                ]

  def load_extra_config( self, config, origin ):
    super().load_extra_config( config, origin )
    self.wrf_met_path   = config.pop( "wrf_met_path", self.wrf_met_path )
    self.wrf_met_folder = config.pop( "wrf_met_folder", self.wrf_met_folder )

  def pre_launch( self ):
    super().pre_launch()
    # met path exists
    self.wrf_met_folder = self.dereference( self.wrf_met_folder )
    self.wrf_met_path = self.resolve_path_exists( self.dereference( self.wrf_met_path ) )
    full_met_path = self.resolve_path_exists( os.path.join( self.wrf_met_path, self.wrf_met_folder ) )

  def pre_run( self ):
    super().pre_run()
    self.setup_dir()
    self.setup_wrf()
    self.setup_metfiles()

  def setup_metfiles( self ):
    full_met_path = self.resolve_path_exists( os.path.join( self.wrf_met_path, self.wrf_met_folder ) )

    # copy over execs, then metfiles, then case to run dir
    prev_exec_raw = self.__exec_raw__
    self.__exec_raw__ = False
    # This should work as everything should be absolute paths
    self.log( "Linking WRF metfiles..." )
    self.execute_subprocess( "ln", [ "-svf", os.path.join( full_met_path, "*" ), self.wrf_run_dir ], verbose=True, shell=True )
    self.__exec_raw__ = prev_exec_raw



class RunWRF( WRFBase ):
  def __init__( self, id ):
    super().__init__( id )
    self.wrf_exec       = "wrf.exe"

    # Override directly or inherit from InitWRF
    self.wrf_case       = None
    self.wrf_case_path  = None
    self.wrf_dir        = None
    self.wrf_run_dir    = None
    self.modify_environ = None
    self.wrf_nml        = None

    # Should we setup the dir again
    self._create_run_dir = True
    self._inherit_dep    = None

    # Not user input
    self.config["command"] = ".sane/wrf/scripts/run_wrf.sh"
    self.config["arguments"] = [
                                "-f", "${{ wrf_run_dir }}",
                                "-r", "${{ wrf_exec }}",
                                "-n", "${{ wrf_nml }}"
                                ]

  def pre_launch( self ):
    # If a dependency can provide us info that we are missing
    for dep_name, dep_info in self.dependencies.items():
      attrs = [
                "wrf_case",
                "wrf_case_path",
                "wrf_dir",
                "wrf_run_dir",
                "modify_environ",
                "wrf_nml"
                ]
      inherit = []
      for attr in attrs:
        if getattr( self, attr ) is None and attr in dep_info["outputs"]:
          setattr( self, attr, dep_info["outputs"][attr] )
          inherit.append( attr )
          self.log( f"Getting '{attr}' info from dependency '{dep_name}'" )

      if "wrf_run_dir" in inherit:
        self._create_run_dir = False

      if len( inherit ) > 0:
        self._inherit_dep = dep_name
        # Only ever inherit from one
        break
    super().pre_launch()

  def pre_run( self ):
    super().pre_run()
    if self._create_run_dir:
      self.setup_dir()
      self.setup_wrf()

      # Copy files needed from dep
      self.setup_input()

  def setup_input( self ):
    # copy over input files
    full_init_path = self.resolve_path_exists( self.dependencies[self._inherit_dep]["outputs"]["wrf_run_dir"] )
    prev_exec_raw = self.__exec_raw__
    self.__exec_raw__ = False
    # This should work as everything should be absolute paths
    self.log( "Linking input files..." )
    self.execute_subprocess( "ln", [ "-svf", os.path.join( full_init_path, "wrf*_d*" ), self.wrf_run_dir ], verbose=True, shell=True )
    self.__exec_raw__ = prev_exec_raw



















































































































# class RunWRF( sane.Action ):
#   def __init__( self, id ):
#     super().__init__( id )
#     self.wrf_case       = None
#     self.wrf_met_folder = "${{ wrf_case }}"
#     self.wrf_case_path  = "${{ host_info.config.run_wrf_case_path }}"
#     self.wrf_met_path   = "${{ host_info.config.run_wrf_met_path }}"
#     self.wrf_dir        = "test/em_real"
#     self.wrf_init_exec  = "real.exe"
#     self.wrf_exec       = "wrf.exe"
#     self.wrf_init_nml   = "${{ wrf_nml }}"
#     self.wrf_nml        = "namelist.input"
#     self.modify_environ = False

#     # Control execution
#     self.mpi_cmd_init   = "${{ mpi_cmd }}"
#     self.mpi_cmd_wrf    = "${{ mpi_cmd }}"
#     self.mpi_cmd        = "mpirun -np ${{ mpi_ranks }}"
#     self.use_mpi        = True
#     self.use_omp        = False
#     self.mpi_ranks      = "${{ resources.cpus }}"
#     self.omp_threads    = "${{ resources.cpus }}"

#     # Not user input
#     self.config["command"] = ".sane/wrf/scripts/run_wrf.sh"
#     self.config["arguments"] = [
#                                 "-r", "${{ wrf_run_dir }}",
#                                 "-i", "${{ wrf_init_exec }}",
#                                 "-w", "${{ wrf_exec }}",
#                                 "-n", "${{ wrf_nml }}"
#                                 ]
#     self.wrf_run_dir = "./output/${{ wrf_case }}"

#   def load_extra_config( self, config, origin ):
#     self.wrf_run_dir    = config.pop( "wrf_run_dir", self.wrf_run_dir )
#     self.wrf_dir        = config.pop( "wrf_dir", self.wrf_dir )
#     self.wrf_case_path  = config.pop( "wrf_case_path", self.wrf_case_path )
#     self.wrf_met_path   = config.pop( "wrf_met_path", self.wrf_met_path )
#     self.wrf_met_folder = config.pop( "wrf_met_folder", self.wrf_met_folder )
#     self.wrf_case       = config.pop( "wrf_case", None )
#     # Use the init default if not provided
#     # Do not check for execs existing yet as those may be created by other actions
#     self.wrf_init_exec = config.pop( "wrf_init_exec", self.wrf_init_exec )
#     self.wrf_exec      = config.pop( "wrf_exec", self.wrf_exec )
#     self.wrf_init_nml  = config.pop( "wrf_init_nml", self.wrf_init_nml )
#     self.wrf_nml = config.pop( "wrf_nml", self.wrf_nml )

#     self.mpi_cmd_init   = config.pop( "mpi_cmd_init", self.mpi_cmd_init )
#     self.mpi_cmd_wrf    = config.pop( "mpi_cmd_wrf",  self.mpi_cmd_wrf )
#     self.mpi_cmd        = config.pop( "mpi_cmd",      self.mpi_cmd )
#     self.use_mpi        = config.pop( "use_mpi",      self.use_mpi )
#     self.use_omp        = config.pop( "use_omp",      self.use_omp )
#     self.mpi_ranks      = config.pop( "mpi_ranks",    self.mpi_ranks )
#     self.omp_threads    = config.pop( "omp_threads",  self.omp_threads )

#     self.modify_environ    = config.pop( "modify_environ",  self.modify_environ )
#     super().load_extra_config( config, origin )

#   def pre_launch( self ):
#     # Preflight checks
#     self.push_logscope( "pre_launch" )

#     # case path and case exist, force assignment check
#     self.wrf_case_path  = self.resolve_path_exists( self.dereference( self.wrf_case_path ) )
#     self.wrf_case       = self.dereference( self.wrf_case )
#     self.wrf_met_folder = self.dereference( self.wrf_met_folder )
#     if self.wrf_case is None:
#       msg = "No case provided"
#       self.log( msg, level=50 )
#       raise ValueError( msg )
#     # case nml exist
#     full_case_path = self.resolve_path_exists( os.path.join( self.wrf_case_path, self.wrf_case ) )
#     self.wrf_init_nml  = self.dereference( self.wrf_init_nml )
#     self.wrf_nml       = self.dereference( self.wrf_nml )
#     self.file_exists_in_path( full_case_path, self.wrf_nml )

#     # met path exists
#     self.wrf_met_path = self.resolve_path_exists( self.dereference( self.wrf_met_path ) )
#     full_met_path = self.resolve_path_exists( os.path.join( self.wrf_met_path, self.wrf_met_folder ) )

#     self.wrf_nml = self.dereference( self.wrf_nml )

#     if self.use_mpi:
#       self.log( f"Adding MPI command to arguments for init : '{self.mpi_cmd_init}'" )
#       self.log( f"Adding MPI command to arguments for wrf  : '{self.mpi_cmd_wrf}'" )
#       self.config["arguments"].extend( [ "-q", self.mpi_cmd_init ] )
#       self.config["arguments"].extend( [ "-p", self.mpi_cmd_wrf ] )
#     if self.use_omp:
#       self.log( f"Adding OMPTHREADS count to arguments : '{self.omp_threads}'" )
#       self.config["arguments"].extend( [ "-o", self.omp_threads ] )

#     if self.use_mpi and self.use_omp:
#       if self.mpi_ranks == self.omp_threads and self.mpi_ranks == "${{ resources.cpus }}":
#         msg = "Directly set the MPI ranks and ompthreads for this action instead of default '${{ resources.cpus }}'"
#         self.log( msg, level=40 )
#         raise Exception( msg )

#     self.pop_logscope()

#   def pre_run( self ):
#     # Now check for things that should be here for sure since any dependencies would be 
#     # finished by now
#     self.push_logscope( "pre_run" )
#     full_case_path = self.resolve_path_exists( os.path.join( self.wrf_case_path, self.wrf_case ) )
#     full_met_path = self.resolve_path_exists( os.path.join( self.wrf_met_path, self.wrf_met_folder ) )

#     # build location exists
#     self.log( self.dependencies )
#     self.wrf_dir = self.dereference( self.wrf_dir )
#     self.wrf_dir = self.resolve_path_exists( self.wrf_dir )

#     # execs exist
#     self.file_exists_in_path( self.wrf_dir, self.wrf_init_exec )
#     self.file_exists_in_path( self.wrf_dir, self.wrf_exec )

#     # OK! Create run dir
#     self.wrf_run_dir = self.dereference( self.wrf_run_dir )
#     self.wrf_run_dir = self.resolve_path( self.working_directory, self.wrf_run_dir )
#     if os.path.isdir( self.wrf_run_dir ):
#       self.log( f"Cleaning '{self.wrf_run_dir}'" )
#       shutil.rmtree( self.wrf_run_dir )
#     os.makedirs( self.wrf_run_dir, exist_ok=True )

#     # copy over execs, then metfiles, then case to run dir
#     prev_exec_raw = self.__exec_raw__
#     self.__exec_raw__ = False
#     # This should work as everything should be absolute paths
#     self.log( "Linking WRF executables..." )
#     self.execute_subprocess( "ln", [ "-svf", os.path.join( self.wrf_dir, "*" ), self.wrf_run_dir ], verbose=True, shell=True )

#     self.log( "Linking WRF metfiles..." )
#     self.execute_subprocess( "ln", [ "-svf", os.path.join( full_met_path, "*" ), self.wrf_run_dir ], verbose=True, shell=True )

#     self.log( "Copying WRF case files..." )
#     self.execute_subprocess( "cp", [ "-v", "--remove-destination", os.path.join( full_case_path, "*" ), self.wrf_run_dir ], verbose=True, shell=True )

#     if self.modify_environ:
#       self.log( "Adding to LD_LIBRARY_PATH..." )
#       os.environ["LD_LIBRARY_PATH"] += f':{os.environ["NETCDF"]}/lib'
#       os.environ["LD_LIBRARY_PATH"] += f':{os.environ["NETCDF"]}/lib64'

#     self.pop_logscope()
#     self.__exec_raw__ = prev_exec_raw


# class RunWRFRestart( RunWRF ):
#   def __init__( self, id ):
#     super().__init__( id )
#     self.wrf_restart_nml = "namelist.input.restart"
#     self.wrf_diff_exec = "./external/io_netcdf/diffwrf"
#     self.hist_comparisons = 1

#   def load_extra_config( self, config, origin ):
#     self.wrf_restart_nml = config.pop( "wrf_restart_nml", self.wrf_restart_nml )
#     self.hist_comparisons = config.pop( "hist_comparisons", self.hist_comparisons )
#     super().load_extra_config( config, origin )

#   def pre_launch( self ):
#     super().pre_launch( )
#     if self.wrf_restart_nml is None:
#       raise ValueError( "No restart namelist specified" )

#     full_case_path = self.resolve_path_exists( os.path.join( self.wrf_case_path, self.wrf_case ) )
#     self.wrf_restart_nml = self.dereference( self.wrf_restart_nml )
#     self.file_exists_in_path( full_case_path, self.wrf_restart_nml )

#     self.wrf_diff_exec = self.dereference( self.wrf_diff_exec )

#   def run( self ):
#     retval = super().run()
#     if retval != 0:
#       return retval

#     arg_dict = {
#                 self.config["arguments"][i] : self.config["arguments"][i+1]
#                 for i in range( 0, len(self.config["arguments"]), 2 )
#               }
#     arg_dict.pop( "-i", None )
#     arg_dict.pop( "-q", None )
#     arg_dict["-n"] = self.wrf_restart_nml
#     arg_dict["-d"] = self.wrf_diff_exec
#     arg_dict["-t"] = self.hist_comparisons
#     self.config["command"] = ".sane/scripts/run_wrf_restart.sh"
#     self.config["arguments"] = list( itertools.chain( *zip( arg_dict.keys(), arg_dict.values() ) ) )

#     self.push_logscope( "run" )
#     self.log( "Running restart namelist now" )
#     self.pop_logscope()

#     # Do it again :)
#     retval = super().run()
#     return retval
