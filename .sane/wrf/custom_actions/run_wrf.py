import os
import copy
import itertools
import shutil

import sane
import wrf.custom_actions.nml as nml_io

class WRFBase( sane.Action ):
  def __init__( self, id ):
    super().__init__( id )

    #: The basename for execution. By default controls the input/output basenames
    self.wrf_case       = None
    #: Root dir to use to locate a set of cases
    self.wrf_case_path  = "${{ host_info.config.run_wrf_case_path }}"
    #: Location of executables
    self.wrf_dir        = "test/em_real"
    #: Location of where to run case. Directory does not need to exist yet
    self.wrf_run_dir    = "./output/${{ wrf_case }}"

    #: For legacy WRF build, on-the-fly modification of environment to allow library finding
    self.modify_environ = False

    #: Name of WRF executable to use
    self.wrf_exec       = None
    #: Namelist to use from ${{ self.wrf_case_path }}/${{ self.wrf_case }} as the namelist to run
    self.wrf_nml        = "namelist.input"

    # Control execution
    #: Exact MPI command to execute. Generally should not need to modify
    self.mpi_cmd        = "mpirun -np ${{ mpi_ranks }}"
    #: Whether to inject MPI command
    self.use_mpi        = True
    #: Whether to inject OpenMP execution
    self.use_omp        = False
    #: Total MPI ranks the run should use
    self.mpi_ranks      = "${{ resources.cpus }}"
    #: Total OpenMP threads the run should use per MPI rank
    self.omp_threads    = "${{ resources.cpus }}"

    #: List of other folders to pull data from, all data symlinked to run dir
    self.extra_data     = []

    #: Namelist patches indexed by basename of namelist file and applied as a recursive
    #: dictionary update to the namelist, e.g. { "my_run.nml" : { "dx" : 1500, "history_interval" : [600, 30] } }
    self.nml_patches    = {}

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
    self.outputs["extra_data"]     = "${{ extra_data }}"
    self.outputs["nml_patches"]    = "${{ nml_patches }}"

  def patch_nml( self, nml ):
    """Rudimentary patching of a namelist

    Uses the sane framework recursive dictionary update to apply changes if a key
    that matches the basename of the ``nml`` is found in ``self.nml_patches``.
    Namelists are read using the ``nml.py`` interface as simple key-value pairs
    under group identifiers. Lists are supported.
    """
    nml_basename = os.path.basename( nml )
    if nml_basename in self.nml_patches:
      nml_dict = nml_io.load_nml( nml )

      nml_dict_patched = sane.helpers.recursive_update( copy.deepcopy(nml_dict), self.nml_patches[nml_basename] )

      if nml_dict == nml_dict_patched:
        self.log( f"Namelist '{nml}' already patched" )
      else:
        self.log( f"Applying patch to '{nml}'" )
        nml_io.dump_nml( nml, nml_dict_patched )

  def load_extra_options( self, options, origin ):
    self.wrf_case       = options.pop( "wrf_case", None )
    self.wrf_case_path  = options.pop( "wrf_case_path", self.wrf_case_path )
    self.wrf_run_dir    = options.pop( "wrf_run_dir", self.wrf_run_dir )
    self.wrf_dir        = options.pop( "wrf_dir", self.wrf_dir )

    # Do not check for execs existing yet as those may be created by other actions
    self.wrf_exec       = options.pop( "wrf_exec", self.wrf_exec )
    self.wrf_nml        = options.pop( "wrf_nml", self.wrf_nml )

    self.mpi_cmd        = options.pop( "mpi_cmd",      self.mpi_cmd )
    self.use_mpi        = options.pop( "use_mpi",      self.use_mpi )
    self.use_omp        = options.pop( "use_omp",      self.use_omp )
    self.mpi_ranks      = options.pop( "mpi_ranks",    self.mpi_ranks )
    self.omp_threads    = options.pop( "omp_threads",  self.omp_threads )

    self.modify_environ    = options.pop( "modify_environ",  self.modify_environ )
    self.extra_data.extend( options.pop( "extra_data",  [] ) )
    sane.helpers.recursive_update( self.nml_patches, options.pop( "nml_patches",  {} ) )
    super().load_extra_options( options, origin )

  def pre_launch( self ):
    """Perform preflight check to make sure case path exists, a case selection is provided,
    the chosen nml exists, and MPI/OpenMP injection.
    """

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
      self.log( f"Adding OMP_NUM_THREADS count to arguments : '{self.omp_threads}'" )
      self.config["arguments"].extend( [ "-o", self.omp_threads ] )

    if self.use_mpi and self.use_omp:
      if self.mpi_ranks == self.omp_threads and self.mpi_ranks == "${{ resources.cpus }}":
        msg = "Directly set the MPI ranks and ompthreads for this action instead of default '${{ resources.cpus }}'"
        self.log( msg, level=40 )
        raise Exception( msg )

  def pre_run( self ):
    """Perform critical checks to ensure WRF exec exists, resolve run dir, and modify env"""
    # Now check for things that should be here for sure since any dependencies would be 
    # finished by now
    full_case_path = self.resolve_path_exists( os.path.join( self.wrf_case_path, self.wrf_case ) )

    # build location exists
    self.wrf_dir = self.dereference( self.wrf_dir )
    self.wrf_dir = self.resolve_path_exists( self.wrf_dir )

    # execs exist
    self.file_exists_in_path( self.wrf_dir, self.wrf_exec )

    # Run location is resolved (we may need to create it)
    self.wrf_run_dir = self.dereference( self.wrf_run_dir )
    self.wrf_run_dir = self.resolve_path( self.working_directory, self.wrf_run_dir )

    # Any extra paths
    self.dereference( self.extra_data )

    if self.modify_environ:
      self.log( "Adding to LD_LIBRARY_PATH..." )
      ld_lib  = os.environ.get( "LD_LIBRARY_PATH", "" )
      ld_lib += f':{os.environ["NETCDF"]}/lib:{os.environ["NETCDF"]}/lib64'
      os.environ["LD_LIBRARY_PATH"] = ld_lib

  def setup_dir( self ):
    """Create fresh run dir if needed"""
    # OK! Create run dir
    self.log( "Setting up run directory..." )
    if os.path.isdir( self.wrf_run_dir ):
      self.log( f"Cleaning '{self.wrf_run_dir}'" )
      shutil.rmtree( self.wrf_run_dir )
    os.makedirs( self.wrf_run_dir, exist_ok=True )

  def setup_wrf( self ):
    """Setup the run directory
    
    Use framework subprocess execution to:
    * Symlink WRF executable directory (including necessary data/run tables)
    * Copy all case files
    * Symlink any additional data
    """
    full_case_path = self.resolve_path_exists( os.path.join( self.wrf_case_path, self.wrf_case ) )

    # copy over execs, then metfiles, then case to run dir
    self.push_exec_raw( False )
    # This should work as everything should be absolute paths
    self.log( "Linking WRF executables..." )
    self.execute_subprocess( "ln", [ "-svf", os.path.join( self.wrf_dir, "*" ), self.wrf_run_dir ], verbose=True, shell=True )

    self.log( "Copying WRF case files..." )
    self.execute_subprocess( "cp", [ "-v", "--remove-destination", os.path.join( full_case_path, "*" ), self.wrf_run_dir ], verbose=True, shell=True )

    if len( self.extra_data ) > 0:
      self.log( "Linking extra data..." )
      for extra_path in self.extra_data:
        self.execute_subprocess( "ln", [ "-svf", os.path.join( extra_path, "*" ), self.wrf_run_dir ], verbose=True, shell=True )

    self.pop_exec_raw()


class InitWRF( WRFBase ):
  def __init__( self, id ):
    super().__init__( id )
    self.wrf_exec       = "real.exe"

    #: Specific folder to use for WPS metfiles
    self.wrf_met_folder = "${{ wrf_case }}"
    #: Root dir of set of metfile folders
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

  def load_extra_options( self, options, origin ):
    super().load_extra_options( options, origin )
    self.wrf_met_path   = options.pop( "wrf_met_path", self.wrf_met_path )
    self.wrf_met_folder = options.pop( "wrf_met_folder", self.wrf_met_folder )

  def pre_launch( self ):
    """Run base :py:meth:`RunWRF.pre_launch()` and then ensure metfile folder exists"""
    super().pre_launch()
    # met path exists
    self.wrf_met_folder = self.dereference( self.wrf_met_folder )
    self.wrf_met_path = self.resolve_path_exists( self.dereference( self.wrf_met_path ) )
    full_met_path = self.resolve_path_exists( os.path.join( self.wrf_met_path, self.wrf_met_folder ) )

  def pre_run( self ):
    """Run base :py:meth:`RunWRF.pre_run()` and then create and setup the run directory, symlink metfiles, and patch namelist"""
    super().pre_run()
    self.setup_dir()
    self.setup_wrf()
    self.setup_metfiles()

    self.patch_nml( os.path.join( self.wrf_run_dir, self.wrf_nml ) )

  def setup_metfiles( self ):
    """Symlink metfiles"""
    full_met_path = self.resolve_path_exists( os.path.join( self.wrf_met_path, self.wrf_met_folder ) )

    # copy over execs, then metfiles, then case to run dir
    self.push_exec_raw( False )
    # This should work as everything should be absolute paths
    self.log( "Linking WRF metfiles..." )
    self.execute_subprocess( "ln", [ "-svf", os.path.join( full_met_path, "*" ), self.wrf_run_dir ], verbose=True, shell=True )
    self.pop_exec_raw()


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
    """For any empty options inherit values from any dependencies that have these attributes,
    e.g. copy from a linked InitWRF dependency, then run the base :py:meth:`RunWRF.pre_launch()`"""
    # If a dependency can provide us info that we are missing
    for dep_name, dep_info in self.dependencies.items():
      attrs = [
                "wrf_case",
                "wrf_case_path",
                "wrf_dir",
                "wrf_run_dir",
                "modify_environ",
                "wrf_nml",
                "extra_data"
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
    """Run base :py:meth:`RunWRF.pre_run()` and then create/setup run directory if different from paired InitWRF, and patch namelist"""
    super().pre_run()
    if self._create_run_dir:
      self.setup_dir()
      self.setup_wrf()

      # Copy files needed from dep
      self.setup_input()

    self.patch_nml( os.path.join( self.wrf_run_dir, self.wrf_nml ) )

  def setup_input( self ):
    """Symlink input files from InitWRF if run directory is different"""
    # copy over input files
    full_init_path = self.resolve_path_exists( self.dependencies[self._inherit_dep]["outputs"]["wrf_run_dir"] )
    self.push_exec_raw( False )
    # This should work as everything should be absolute paths
    self.log( "Linking input files..." )
    self.execute_subprocess( "ln", [ "-svf", os.path.join( full_init_path, "wrf*_d*" ), self.wrf_run_dir ], verbose=True, shell=True )
    self.pop_exec_raw()


class RunWRFRestart( RunWRF ):
  def __init__( self, id ):
    super().__init__( id )
    #: The restart namelist to use for the comparison restart run
    self.wrf_restart_nml = "namelist.input.restart"
    #: Location of the diffwrf executable to use when comparing domain outputs
    self.wrf_diff_exec = "./external/io_netcdf/diffwrf"
    #: Number of history files to compare per domain, starting from latest
    self.hist_comparisons = 1

  def load_extra_options( self, options, origin ):
    self.wrf_restart_nml = options.pop( "wrf_restart_nml", self.wrf_restart_nml )
    self.hist_comparisons = options.pop( "hist_comparisons", self.hist_comparisons )
    super().load_extra_options( options, origin )

  def pre_launch( self ):
    super().pre_launch( )
    if self.wrf_restart_nml is None:
      raise ValueError( "No restart namelist specified" )

    full_case_path = self.resolve_path_exists( os.path.join( self.wrf_case_path, self.wrf_case ) )
    self.wrf_restart_nml = self.dereference( self.wrf_restart_nml )
    self.file_exists_in_path( full_case_path, self.wrf_restart_nml )

    self.wrf_diff_exec = self.dereference( self.wrf_diff_exec )

  def pre_run( self ):
    super().pre_run()
    self.patch_nml( os.path.join( self.wrf_run_dir, self.wrf_restart_nml ) )

  def run( self ):
    retval = super().run()
    if retval != 0:
      return retval

    arg_dict = {
                self.config["arguments"][i] : self.config["arguments"][i+1]
                for i in range( 0, len(self.config["arguments"]), 2 )
              }
    arg_dict["-n"] = self.wrf_restart_nml
    arg_dict["-d"] = self.wrf_diff_exec
    arg_dict["-t"] = self.hist_comparisons
    self.config["command"] = ".sane/wrf/scripts/run_wrf_restart.sh"
    self.config["arguments"] = list( itertools.chain( *zip( arg_dict.keys(), arg_dict.values() ) ) )

    self.log( "Running restart namelist now" )

    # Do it again :)
    retval = super().run()
    return retval
