# Workflow
These tests follow the mantra of _"CI/CD framework-agnostic"_ such that they can
more or less be run anywhere as long as you have the necessary data/case files
and create or run supporting environments.

Provided is a default configuration for Derecho. Datafiles for Derecho runs are
provided at:
```
/glade/campaign/mmm/wmr/testing/
```

Documentation for this new framework can be found at:\
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

usage: sane_view [-h] {usage,status,state,logs} ...

positional arguments:
  {usage,status,state,logs}
    usage               View resource usage
    status              View action status
    state               View action state
    logs                View action logs
```

For instance, to get a list of all logs for tests that have failed:
```bash
sane_view logs --errors

  restart_nwp_diag                  : /glade/work/aislas/wrf-model/wrf/log/restart_nwp_diag.log
  restart_km_opt_3                  : /glade/work/aislas/wrf-model/wrf/log/restart_km_opt_3.log
  restart_basic                     : /glade/work/aislas/wrf-model/wrf/log/restart_basic.log
  restart_w_damping                 : /glade/work/aislas/wrf-model/wrf/log/restart_w_damping.log
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
            └── wrf_coop.py     #< Python module that sets up the WRF Coop em_real* tests
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


## WRF Coop
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
