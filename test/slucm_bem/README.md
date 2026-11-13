# Native SLUCM+BEM regression

Run from a WRF checkout containing the original PR commit
`6099d9f632e204cc8ed4b01e1a8adba1da753a47` in its local Git history:

```bash
python3 -I test/slucm_bem/run_native.py --fc ifx \
  --fflags='-O2 -traceback -check bounds -fp-model=strict'
```

`ifort` and `gfortran` are also supported. Python 3.6 or later and its
standard library suffice. No MPI, NetCDF, external meteorological input or
network access is required. The default flags use `-O2`, bounds checks and
traceback. Override the additional flags with `--fflags` when necessary;
the required preprocessing/free-source flags are retained.

The explicit Intel floating-point flags above are the verified configuration
for ifx 2024.0.2. Its initial run with the default flags, without an explicit
floating-point model, failed the exact SBEM0/three-category comparison. That
failure remains recorded; no comparison tolerance was relaxed. The runner
defaults remain unchanged. GNU Fortran 14.2 passed the recorded O0 and O2
configurations; see [the GNU record](validation_gfortran_20261113.json).

The runner creates a new temporary output directory and prints its
`native_report.json` path. Use `--output-dir /path/to/new/directory` for an
explicit destination. An existing directory is rejected to protect earlier
results. Source files, parameter tables, compiler identity and flags are
recorded with the commands and complete logs. A changed source during the
run is a failure.

## What is compiled

The complete current `module_sf_urban.F`, current
`module_sf_slucm_bem.F`, actual `module_model_constants.F` and actual
`URBPARM.TBL` / `URBPARM_LCZ.TBL` are used. Only logging/fatal-error services
and an unused namelist lookup are replaced by `wrf_test_stubs.F90`.
No physics routine is mocked or translated to Python.

The original two physics modules are read with `git show` and checked
against fixed SHA-256 values. The BEM reference is additionally compiled
under a different module name. The original urban module is compiled in a
separate executable. `--baseline-ref` can select another local Git object
only when its file contents match the same fixed reference hashes.

## Required checks

| Check | Coverage |
|---|---|
| BEM controls | 42 cases, including 17 exact comparisons of 19 diagnostics with the fixed reference, independent threshold timing, heat/water budgets, HVAC identities and zero/extremely dry states |
| Full urban/BEM calls | 28 cases with the three-category table and 56 with the LCZ table; WRF time steps of 6, 30, 60 and 180 seconds |
| Coupling | Same-step returned/source residuals, current `TC/QC` used by ventilation, indoor heat/water budgets, dry latent flux, standard-canyon top sensible-flux closure |
| State restoration | Exact one-trial reconstruction of 147 real argument values, including all continuing states; wet impervious surfaces, green-roof water/temperature states and distributed morphology |
| Existing paths | 56 reference records of 147 values each must be byte-identical: BEM disabled and BEM enabled with HVAC/ventilation exchange disabled; three-category, LCZ and distributed calls included |
| Rejection | Six standalone BEM failure modes and five urban failure modes under each table, including invalid humidity, a zero-conductance all-roof canyon and inadmissible predicted indoor states |
| Continuation | Serialize/restore the complete single-column call state and verify the next step after an intervening unrelated cell and a reset step counter |

The distributed-aerodynamics scheme retains its direct `TA/QA` reference
for surface exchange. Its tests use local geometric normalization, source
consistency, indoor budgets and exact state replay. They do not assume its
diagnostic `TC/QC` represents the standard canyon control volume.

Expected completion:

```text
WRF_SLUCMBEM_NATIVE_PASS
42 BEM + 84 urban cases; 16 expected failure exits; 56 exact baseline records x 147 values
```

## Limits

These are compiled single-column numerical tests with synthetic forcing
and `TREEOPTION=0`. They do not compile the entire WRF system, invoke
Noah/Noah-MP drivers or MPI, or test WRF restart files and halo exchanges.
The serialization test is a unit-level state replay, not a WRF restart
acceptance test. Some deliberately strong synthetic HVAC conditions can
produce high canyon temperatures while still satisfying the tested
numerical budgets.

The report keeps `physical_acceptance=false`, `full_wrf_build=false`,
`noah_or_noahmp_driver_integration=false`, `mpi_execution=false` and
`wrf_restart_io=false`. Production-compiler and coupled WRF verification
must be reported separately.

## Separately completed coupled verification

[The Intel and WRF record](validation_wrf_ifx_20261113.json) summarizes the
strict Intel native run, the incremental O3/precise WRF build and seven
32-rank coupled runs. It records exact disabled-BEM regression, exact saved
post-split history payloads, baseline reproduction of the retained restart
residuals, and a winter Noah/LCZ standard-canyon run with active ventilation
and heating. The native report's scope flags continue to describe only this
native suite; the additional evidence does not retroactively change them.

See [the exchange documentation](../../doc/SLUCM_BEM_ventilation.md) for the
configuration matrix and limits. Full restart-file equality remains false.
Physical acceptance remains false, including the low canyon temperature
retained in the winter result. Earlier long simulations and figures predate
this correction and are not new validation results.
