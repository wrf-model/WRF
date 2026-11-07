# Workflow
These tests follow the mantra of _"CI/CD framework-agnostic"_ such that they can
more or less be run anywhere as long as you have the necessary data/case files
and create or run supporting environments.

Provided is a default configuration for Derecho. Datafiles for Derecho runs are
provided at:
```
/glade/campaign/mmm/wmr/testing/
```

Documentation for tooling to run this new framework can be found at:\
https://sane-workflows.readthedocs.io/en/latest/

One could run these tests on Derecho using the following commands (inside a WRF repo clone):
```bash
# Assuming a bash-like shell
python3 -m venv .venv/wrf_testing
source .venv/wrf_testing/bin/activate
python3 -m pip install --pre sane-workflows
# Runs the em_real test case
sane_runner --path .sane/ --actions em_real --run
```

Note that after you do this once, you can reactivate the environment in new terminal
sessions by sourcing the same `activate` script.
```bash
# Once setup, this is the only command needed to make the commands available
source .venv/wrf_testing/bin/activate
```

See https://docs.python.org/3/library/venv.html#creating-virtual-environments for
more details on Python virtual environments.

Once run, the results are printed to the terminal and stored in the `log/` folder
as:
| File               | Contents |
|--------------------|--------|
|`log/runner.log`    | stdout |
|`log/results.log`   | JUnit  |
|`log/<name>.log`    | Test <name> full log  |
|`log/<name>.runlog` | Test <name> exec log  |

At any point in time during workflow execution, a helper script can also be used
to inspect the state, status, and logs:
```bash
sane_view --help

usage: sane_view.py [-h] {usage,status,state,logs,summary} ...

positional arguments:
  {usage,status,state,logs,summary}
    usage               View resource usage
    status              View action status
    state               View action state
    logs                View action logs
    summary             View workflow summary
```

For instance, to get a list of all logs for tests that have failed:
```bash
sane_view logs --errors

  restart_nwp_diag                  : /glade/work/aislas/wrf-model/wrf/log/restart_nwp_diag.log
  restart_km_opt_3                  : /glade/work/aislas/wrf-model/wrf/log/restart_km_opt_3.log
  restart_basic                     : /glade/work/aislas/wrf-model/wrf/log/restart_basic.log
  restart_w_damping                 : /glade/work/aislas/wrf-model/wrf/log/restart_w_damping.log
```


To view what will be run, use the `--list` flag and optionally the `--view_graph`
flag to see dependency order. For instance:
```bash
sane_runner -p .sane/ -f "restart_(basic|nwp_diag)_make" --view_graph --list
...
2026-09-28 09:34:45 INFO     [sane_runner]            Listing actions:
2026-09-28 09:34:45 INFO     [orchestrator]             restart_basic_make     restart_nwp_diag_make  
2026-09-28 09:34:45 INFO     [orchestrator]           
2026-09-28 09:34:45 INFO     [orchestrator]           Action Graph:
2026-09-28 09:34:45 INFO     [orchestrator]             • build_make_em_real_gnu_debug_dm
2026-09-28 09:34:45 INFO     [orchestrator]             ┡➢• restart_basic_init_make
2026-09-28 09:34:45 INFO     [orchestrator]             ┡➢│╌• restart_nwp_diag_init_make
2026-09-28 09:34:45 INFO     [orchestrator]             ┡➢┺➢│╌• restart_basic_make ✧
2026-09-28 09:34:45 INFO     [orchestrator]             ┗➢╌╌┺➢• restart_nwp_diag_make ✧
2026-09-28 09:34:45 INFO     [sane_runner]            Finished
...
```

The command line graph rendition may seem confusing at first, but it may make more
sense if you use this reference:

https://sane-workflows.readthedocs.io/en/latest/user_guide/running.html#view-graph

The gist of it is dependencies are read horizontally for all `┗➢`, and the parent Action is followed
vertically until the `•` (text to the right is the Action name).

Looking at the above example in more detail for `restart_nwp_diag_make`, dependency
connections were replaced with `O` and the actual parent Action noted with `X`
to help identifcation with respect to other Actions. Thus, we can see that
`restart_nwp_diag_make` depends on both `build_make_em_real_gnu_debug_dm`
and `restart_nwp_diag_init_make`.
```bash
X build_make_em_real_gnu_debug_dm
┡➢• restart_basic_init_make
┡➢│╌X restart_nwp_diag_init_make
┡➢┺➢│╌• restart_basic_make ✧
O➢╌╌O➢• restart_nwp_diag_make ✧
```


# Structure
The tests are now written in the [SANE Workflows](https://github.com/islas/sane_workflows) framework, which solves most of the issues faced by the other setups. Data is still spread across multiple locations, but that is separate from the testing code.

The structure of the tests is as follows:
```
.sane/                          #< The root directory in WRF where the testing code is kept
└── wrf                         #< A subfolder to make all python-imports look like `import wrf`
    ├── custom_actions
    │   └── run_wrf.py          #< A module that has our custom reusable classes
    |                           #< to setup initial conditions and model runs
    ├── hosts
    │   ├── derecho_envs.jsonc  #< The environments that derecho.jsonc has - separate for clarity
    │   └── derecho.jsonc       #< Definition of derecho HPC system for this framework
    ├── scripts                 #< A subfolder to house all our shell helper scripts that
    |   |                       #< do the bulk of the work
    │   ├── buildCMake.sh
    │   ├── buildMake.sh
    │   ├── compare_wrf.sh      #< Use diffwrf to compare two runs
    │   ├── run_init.sh         #< Configurable to run initial conditions (em_real.exe or ideal.exe)
    │   ├── run_wrf_restart.sh  #< Runs wrf.exe again in previous run folder and compares history
    │   └── run_wrf.sh          #< Runs wrf.exe
    └── tests                   #< Where our tests live
        ├── builds
        │   └── builds.py       #< Python module that sets up ALL our compilation tests (make + cmake)
        └── regtests
            ├── restart.py      #< Python module that sets up the WRF restart feature tests
            └── wrf_coop.py     #< Python module that sets up the WRF Coop tests
```

# Tests

## Builds
| Builds          | |
| --------------- | ------------- |
| GNU             | Intel classic*  |
| PGI/nvhpc       | Intel oneAPI    |

\* Intel classic _can_ work if you somehow get an appropriate environment set up


All builds have permutations of:
* Make/CMake
* Debug/Release
* SM/DM
* EM_REAL/EM_FIRE/EM_B_WAVE

A couple chemistry builds exists for the GNU environment, using the permutations:
* Make/CMake
* chem/kpp
* serial/mpi


## WRF Coop
The WRF Coop Tests were originally created in a CentOS container. These ports aim to
isolate the core logic from the container environment concerns. Each test is mirrored
in Make and CMake build systems so that both are tested.

### WRF EM_REAL
The following tests cover the WRF Coop Test port:
| Real Test Cases  |  |
| ------------- | ------------- |
| em_real   | em_realG  |
| em_realA  | em_realH* |
| em_realB  | em_realI  |
| em_realC  | em_realJ  |
| em_realD  | em_realK  |
| em_realE  | em_realL  |
| em_realF  |   |

\* em_realH does not compare against OpenMP

Each test is composed of a serial initial condition for that case and a set of
WRF runs for serial, SM (OpenMP), and DM (MPI). All are based on the GNU Make
build for `em_real`, debug and SM+DM. To achieve serial, SM, and DM runs launch
environment is modified to select 1 rank/1 thread, 1 rank/N threads, and N rank/1 thread
respectively.

More information on originating source can be found here:\
https://github.com/wrf-model/wrf-coop/blob/master/README_user.md

### WRF Chemistry

The chem tests are also ported from WRF Coop.
* `em_chem` ports namelists `1`, `2`, and `5`
* `em_chem_kpp` ports namelist `120`

Each test is composed of a serial and MPI full runs with Make and CMake.
The Make build makes use of command-line argument configuration instead of the
old environment variable setup.
Cmake uses the supported `ENABLE_CHEM=ON` and `ENABLE_KPP=ON`.

Select `em_chem` or `em_chem_kpp` for both build systems; append `_make` or
`_cmake` to select just one.

KPP also needs `flex` and a yacc-compatible parser on the host environment's `PATH`;
CMake KPP builds require Bison in lieu of yacc. CMake must be able to discover the
FLEX installation (use the host's `CMAKE_PREFIX_PATH` for non-system installs).
The host GNU environment sets `FLEX_LIB_DIR` to locate `libfl`, and
`YACC` supplies the parser command including `-d`. Override these tool
settings in a host environment patch when necessary.

```bash
# Run em_chem tests on derecho
sane_runner -p .sane -sh derecho -a em_chem -r
```
A PBS dry-run does not verify scheduler acceptance, installed KPP tools,
case data availability, or numerical results.

## WRF Restart
The following tests cover the WRF Restart feature tests:
| Restart Cases  |  |
| ------------- | ------------- |
| basic      | km_opt_1  |
| dfi        | km_opt_2  |
| diff_opt_2 | km_opt_3  |
| nwp_diag  | nest_starts_later  |
| w_damping |   |

Each test is composed of a serial initial condition for that case and then a DM (MPI)
WRF run of the test case followed by restart run using the restart file generated.
Comparison is done between the last WRF outputs between the initial run and the
restart run.

More information on originating source can be found here:\
https://github.com/wrf-model/wrf_feature_testing/blob/main/README.md

# Adding new WRF tests

## New Build Configuration
Build *actions* are implemented with the default framework and helper scripts found
in `.sane/wrf/scripts/`. To add a new build, please add a new configuration to the
same file (`.sane/wrf/tests/builds/builds.py`) and pass the appropriate flags
to the script using the framework. Refer to the executables in the scripts
directory for detailed information.

## New WRF run

WRF runs are implemented using both helper scripts and custom Python classes interfacing
with the testing framework. The helper scripts manage the generalized execution,
echoing of output, and basic confirmation that the execution was successful. The
custom classes manage passing necessary arguments to the scripts as well as
everything else involved with run setup:
* setting up run directories
* linking metfiles
* copying and selecting specific namelists
* patching namelists
* chaining information between runs such as ideal/real to wrf, or wrf to a restart

To add a new WRF run in Python add a new function that is registered to the framework
and create the type of WRF run:
```python
import sane

import wrf.custom_actions.run_wrf as run_wrf

# Add register decorator
@sane.register
def my_run( orch ):
  # Create the run
  setup = run_wrf.InitWRF( "my_setup" )
  ...
  # Make sure to add to orch
  orch.add_action( setup )
```

To add a new WRF in the JSON config add a new action with the appropriate type:
```jsonc
{
  "actions" :
  {
    "my_setup" :
    {
      "type" : "wrf.custom_actions.run_wrf.InitWRF"
    }
  }
}
```

Regardless of how you plan to create the run, the following information is generally
the same.
1. Set the build and environment that this run should use. The environment the build uses should match
   and is dependendent on what the selected *host* supports
2. Set the required `wrf_case` option
3. Set any of the other optional settings. For JSON, use just the attribute name
   under the action instead of the ``.`` dot operator for Python, e.g.
    ```jsonc
    // ...
    "my_setup" : 
    {
      "type" : "wrf.custom_actions.run_wrf.InitWRF",
      "wrf_case" : "foobar"
    }
    ```

    vs
    ```python
    #...
    setup = run_wrf.InitWRF( "my_setup" )
    setup.wrf_case = "foobar"
    ```

    Current options are:
    ```python
    #### Applicable to all types of runs ####
    #: Location of executables
    self.wrf_dir        = "test/em_real"
    #: Location of where to run case. Directory does not need to exist yet
    self.wrf_run_dir    = "./output/${{ wrf_case }}"
    #: For legacy WRF build, on-the-fly modification of environment to allow library finding
    self.modify_environ = False
    #: Name of WRF executable to use
    self.wrf_exec       = None
    #: Namelist to use for execution
    self.wrf_nml        = "namelist.input"
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
    #: Namelist patch applied as a recursive dictionary update to the namelist with groups,
    #: e.g. { "domains" : { "dx" : 1500 }, "time_control" : { "history_interval" : [600, 30] } }
    self.nml_patches    = {}

    #### FOR InitWRF ####
    #: Folder folder to use for WPS metfiles 
    self.wrf_met_path   = "${{ host_info.config.run_wrf_met_path }}"

    #### FOR RestartWRF ####
    #: The restart namelist to use for the comparison restart run
    self.wrf_restart_nml = "namelist.input.restart"
    #: Location of the diffwrf executable to use when comparing domain outputs
    self.wrf_diff_exec = "./external/io_netcdf/diffwrf"
    #: Number of history files to compare per domain, starting from latest
    self.hist_comparisons = 1
    #: Namelist patch applied as a recursive dictionary update to the namelist with groups,
    #: e.g. { "domains" : { "dx" : 1500 }, "time_control" : { "history_interval" : [600, 30] } }
    self.nml_restart_patch    = {}
    ```


A good way to setup your metfiles and namelists cases is to set the values at the
*host* ``config`` level as these files are often platform/device specific. To do
in the *host* `config` add a dictionary that contains `run_wrf_case_path` and `run_wrf_met_path`,
such as:
```jsonc
// from derecho config
{
  "hosts" :
  {
    "derecho" :
    {
      // ...
      "config" :
      {
        "wrf_coop" :
        {
          "run_wrf_case_path" : "/glade/campaign/mmm/wmr/testing/wrf/cases/SCRIPTS/Namelists/weekly/",
          "run_wrf_met_path" : "/glade/campaign/mmm/wmr/testing/wrf/data/wrf_coop/Data/"
        }
      }
    }
  }
}
```

Then, in your *action* option setup, use these paths for your runs. This allows
you to group sets of runs together to build out a suite of tests.
```python
# Python example
init_wrf.wrf_case_path   = "${{ host_info.config.wrf_coop.run_wrf_case_path }}"
init_wrf.wrf_met_path    = "${{ host_info.config.wrf_coop.run_wrf_met_path }}"
```

# Adding support for Custom Host
For custom hosts, include a local host patch directory with
`-p <your host patch> -sh <your host>`. Add or set environment variables as needed
using `"env_vars"` with supported commands, or an environment script via `"env_scripts"`

Make sure for the tests you plan to run you provide the host `"config"` information
that the tests expect. 
* `wrf_coop` tests expect `config.wrf_coop` to have `run_wrf_case_path` and `run_wrf_met_path`
  - Case files are from: https://github.com/wrf-model/SCRIPTS/tree/master/Namelists/weekly
  - Met files are from: https://www2.mmm.ucar.edu/wrf/dave/data_smaller.tar.gz
* `wrf_restart` tests expect `config.wrf_restart` to have `run_wrf_case_path` and `run_wrf_met_path`
  - Case files are from: https://github.com/wrf-model/wrf_feature_testing/tree/main/cases
  - Met files are from: https://www2.mmm.ucar.edu/wrf/dave/feature_data.tar.gz
