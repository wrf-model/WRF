# Green-roof conduction coupled to SLUCM+BEM

## Change and activation

When `SBEMOPTION=1`, `GROPTION=1` and `FGR>0`, the vegetated roof now
conducts heat through its substrate and concrete roof to the BEM room.
Previously its thermal column used a prescribed deep boundary, while the
whole BEM roof load came from the separate bare-roof column. Surface heat
and moisture fluxes alone did not provide this direct roof–room connection.

This addition uses the existing four `TGRL` cells: three substrate cells and
one concrete cell. `DZGR` is in metres. The fourth cell uses the selected
urban category's `CAPR` and `AKSR`, converted from the urban core's cgs
units to SI. It is not a fourth soil cell. Its thickness and material
properties need to represent the intended roof construction; they should
not be inferred from the bare-roof layer count alone.

The input table must have `TS_SCHEME=1`, four roof layers, finite `FGR`
between zero and one, and four finite positive `DZGR` thicknesses. The
existing prohibition on combining green roofs with
`slucm_distributed_drag` is retained. Standard three-category and LCZ
configurations are covered by the new native tests. These tests use
`TREEOPTION=0`, consistent with the current WRF PR validation scope.

The parameter-table defaults remain `GROPTION=0` and `FGR=0`. For a
separate green-roof experiment, select `GROPTION: 1`, `FGR: 0.5` (or the
intended fraction) and `SBEMOPTION: 1` in the active urban parameter table.
Compare fractions 0, 0.5 and 1 with otherwise identical inputs.

## Thermal and BEM balances

`module_sf_greenroof_bem.F` advances the four finite-volume cells with a
backward-Euler solve in double precision. Each intercell resistance is the
sum of the two adjacent half-layer resistances. The lower boundary is the
half-concrete-layer resistance in series with the indoor convective and
linearized radiative films. The upper boundary is the iterated green-roof
surface temperature. The surface Newton solve uses the analytical
derivative of this implicit column solution.

Natural convection uses the existing BEM `cal_chtc` function at the inner
surface. Its coefficient is determined from the old state and held fixed
over the step, as is the existing radiative linearization. Each trial uses
the same original layer temperatures and moisture state. A rejected outer
urban/BEM trial cannot advance the column a second time.

The heat leaving the bottom of the column is split into convection and
radiation using those same films. The two BEM roof loads are mixed per
unit roof area, exactly once:

```text
Qroof_conv = (1-FGR) * Qbare_conv + FGR * Qgreen_conv
Qroof_rad  = (1-FGR) * Qbare_rad  + FGR * Qgreen_rad
```

The BEM subsequently applies its existing building/roof/floor geometry.
The green contribution must not be multiplied by `FGR` a second time.
The previous inner-surface temperature supplied to the existing comfort
diagnostics uses the same roof-area mixture. The bare-roof and wall
thermal updates, HVAC equations and urban tree physics are unchanged.

`GREENROOF_DIAG` is an optional trailing argument to `urban` and
`urban_step`. It exposes instantaneous column/interface fluxes, layer
capacities and conductivities, actual mixed BEM loads, and residuals for
native tests. It introduces no continuing state or Registry variable.
The column solver's optional `state_valid` lets the WRF caller report
invalid input through `FATAL_ERROR`, instead of a serial `STOP` inside an
MPI run.

The column conserves energy with the capacities frozen for each step.
It does not model enthalpy transported by moisture movement. The reported
double-precision solver residual and the residual reconstructed from the
stored single-precision layer temperatures are separate: the latter has
a timestep-dependent rounding bound. This is not a claim of whole-model
observational accuracy or timestep convergence.

## Build and continuing states

The new module is registered in `phys/Makefile`, `phys/CMakeLists.txt`
and the `module_sf_urban.o` dependencies in `main/depend.common`. A full
rebuild must include it; replacing an old executable is necessary before
running the new physics.

Noah-LSM and Noah-MP already pass and save the green-roof temperature,
moisture and interception states through the shared urban routine. The
WRF Registry contains restart entries for `TGR_URB2D`, `TGRL_URB3D`,
`SMR_URB3D`, `CMCR_URB2D`, the green-roof exchange coefficients, and the
indoor `TIN_URB2D`/`QIN_URB2D` states. This change adds no new prognostic
state and needs no additional Noah-MP call argument or submodule update.
This source-path inspection does not replace a coupled restart test.

## Native verification

Run the existing controls/exchange suite and the new roof suite from the
WRF checkout. The runners use Python's standard library and a Fortran
compiler; no meteorological files or MPI launch is needed:

```bash
python3 -I test/slucm_bem/run_native.py --fc gfortran
python3 -I test/slucm_bem/run_greenroof.py --fc gfortran
```

Use the compiler option documented by each runner's `--help` on a system
with a different compiler. Each runner requires a new result directory
when an explicit output location is supplied. The original controls
suite retains its pinned `6099d9f632e204cc8ed4b01e1a8adba1da753a47`
reference. The roof suite compares with
`9a6f9edc660e9574dcd4a51a4ba91c7025f24ea6`, immediately before this
addition. Reference source bytes are SHA256 checked; a directory of
extracted reference sources can be provided using the documented option.

Both runners append `-ffp-contract=off` for GNU Fortran, including after
custom `--fflags`, to compare the reference and candidate using uncontracted
floating-point expressions. The existing controls suite still defaults to
`-O2`; the roof diagnostic suite still defaults to `-O0` with its runtime
checks. The fixed-source checks and byte-exact regression criteria are
unchanged. This policy addresses a reproduced FMA-sensitive difference in
the non-BEM comparison that also occurs with the pre-roof source. It changes
only the native test build commands; full WRF compiler settings and coupled
acceptance remain separate. See the [native test README](../test/slucm_bem/README.md)
for the Mac evidence and the controlled reproduction.

The new suite compiles the complete production urban and BEM modules.
Only WRF infrastructure services are stubbed; the roof, hydrology, urban
and indoor physics are real compiled Fortran. It checks active fractions,
roof/room heat-flow directions, several WRF time steps, standard and LCZ
categories, wet/dew conditions, 72-hour trajectories, layer and column
energy balances, and actual BEM-load mixing. It also compares disabled,
non-BEM green-roof and zero-fraction snapshots with the pre-change core,
and replays serialized single-column states. The independent four-cell
fixture checks an analytical steady resistance solution, transient
conservation, surface derivatives and first-order time refinement.

The scope flags in the reports deliberately leave full WRF build,
Noah/Noah-MP driver integration, MPI execution, WRF restart I/O and
physical acceptance unverified. An identical single-column state replay
is not a WRF restart-file test. Existing coupled evidence from before
this addition remains evidence for those earlier sources only.

## Coupled acceptance still required

For both Noah-LSM and Noah-MP, build this source with the production
compiler and run the following using the same geometry and forcing:

1. Compare old and new executables with `GROPTION=0`; all intended
   saved fields must match the accepted regression criterion.
2. Check `GROPTION=1` with `FGR=0`, 0.5 and 1, including a wet interval,
   a dry interval and the relevant cooling/heating season.
3. Compare a continuous run with a run restarted from its own checkpoint,
   using the same new executable, MPI decomposition and thread count.
   Compare saved times after the split; do not label the initial restart
   timestamp as a new forward integration step.
4. Inspect `TGR/TGRL`, soil water, indoor temperature/humidity, roof loads,
   HVAC energy and surface fluxes. Check time-step sensitivity at the
   actual production land-surface/urban time step and allow spin-up after
   a change in thermal boundary conditions.

This addition does not fix the separately retained high-load HVAC
fixed-point failure: a synthetic category-3 initial room temperature of
307 K, 6-second step and AC fraction 0.6 already fails with the
pre-change core. The targeted roof matrix uses a 299 K room initial
state and retains both heat-flow directions through its roof initial
states. Nor does this change alter the existing outer solver's first
comparison against twice `HUGE`; the full-core debug suite traps invalid
and division-by-zero exceptions, while the isolated column additionally
traps overflow. These limits are recorded rather than hidden by relaxed
numerical acceptance thresholds.
