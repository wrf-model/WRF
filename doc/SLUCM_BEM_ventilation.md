# SLUCM+BEM: conservative building-to-outdoor exchange

## Purpose and provenance

The previous SLUCM+BEM implementation included ventilation in the indoor
sensible and latent loads but omitted its opposite exchange in the outdoor
fluxes and diagnostic canyon balances. It also decided whether HVAC was active
only at the beginning of a time step and unconditionally raised sufficiently
small indoor humidity to `1e-4 kg kg-1` after the update.

This change is based on WRF PR 2452 at
`6099d9f632e204cc8ed4b01e1a8adba1da753a47`. The physical corrections were identified
and tested in ILS at `3e6bda9b4ce1a212b1b4f1b2006fb351ec50f082`, particularly
`docs/slucm_bem_ventilation_ja.md` and its threshold-crossing follow-up. The WRF
port uses WRF's actual time step and geometry. The ILS hourly integration,
MATSIRO driver, Japan land-use scenarios, and T2/Q2 output changes are not part
of this patch.

## Exchange definitions and routing

`QVENT` and `EVENT` are positive into the building, in W per unit canyon length
(`W m-1`); `EVENT` is latent energy, not a water mass flux. With
`A = ROOF_WIDTH2 + ROAD_WIDTH2`, the outdoor-positive exchanges per urban land
area (`W m-2`) are

```text
H_BEM_net  = QFB_URB  - QVENT / A
LE_BEM_net = QFBL_URB - EVENT / A
```

The calculation uses the actual widths passed to BEM, including the widths
derived from distributed morphology. No additional roof fraction, air-
conditioning fraction, canyon fraction, or grid urban fraction is applied.
The existing Noah and Noah-MP drivers apply the urban tile fraction when
combining urban and natural surface fluxes.

`QFB_URB`, `QFBL_URB`, `QFB_CL_URB`, `QFB_HT_URB`, and `ECAC_URB` retain their
HVAC definitions. `QF_URB` and `QFL_URB` remain gross artificial-source
diagnostics. They are not redefined to be ventilation-inclusive net exchange.
The existing QVENT/EVENT diagnostics can be used with the actual BEM geometry
to reconstruct the direct net building exchange.

For the ordinary canyon formulation, the same-step BEM net sources enter
`TC2` and `QC2`. They also enter the final outdoor fluxes once:

```text
SH = roof + wall + road surface fluxes + traffic heat + H_BEM_net
LH = roof + wall + road surface fluxes + external ALH + LE_BEM_net
```

The surface terms are not the already-summed canyon-top flux, so this is not a
second addition of the BEM source. Temperature and humidity diagnostics are
computed after these fluxes are finalized. Water mass flux retains the
existing conversion `LH_KINEMATIC = LH / ELL`.

This change makes HVAC net exchange affect the ordinary canyon's same-step
temperature and humidity, whereas the previous PR added HVAC only to the final
outdoor fluxes. Traffic heat and prescribed external ALH retain their existing
direct atmospheric routing. The port does not import ILS's AHOPTION routing.
`ALHOPTION=0` disables prescribed ALH, not BEM latent exchange or ventilation.

### Distributed aerodynamics

The existing distributed formulation partitions a bulk surface-to-atmosphere
conductance among roof, wall, and road. Its surface fluxes use atmospheric
`TA/QA` directly; its `TC/QC` are diagnostics. This structure and its surface
Jacobians are retained. The new source affects its diagnostic `TC/QC`, BEM,
and total `SH/LH`, with the opposite indoor/outdoor exchange and area conversion
checked explicitly.

The distributed diagnostic canyon is not claimed to become the same energy
control volume as the ordinary canyon. Replacing its `TA/QA` surface references
by `TC/QC` would introduce another resistance and change even zero-source
surface exchange. That is a separate parameterization change.

## Same-step solution and state preservation

The public `urban` routine wraps a single-trial `urban_step`. For SBEMOPTION=1,
it saves all 51 explicitly in/out arguments plus the indoor-temperature state:
52 variables, including arrays, morphology, exchange coefficients, and surface
water stores. Every trial starts from these same states. Only the accepted
trial advances the caller's state. This prevents repeated roof/wall conduction,
indoor integration, precipitation, or evaporation during source iteration.

Two trial sources, sensible and latent net building exchange, are solved with
a relaxed first step and a two-variable Broyden iteration. The initial source
is zero on every call; no preceding time step's source or hidden saved state
is used. A source which already meets the exchange tolerance is evaluated
again without perturbation if the canyon-state confirmation is still pending.

Acceptance requires both source residuals to satisfy

```text
abs(returned - supplied) <= 0.01 + 1e-5 * max(abs(returned), abs(supplied)) W m-2
```

and consecutive valid trials to differ by no more than `0.002 K` in TC and
`2e-7 kg kg-1` in QC. These are iteration differences, not limits on physical
time evolution. Sources passed to the core are the actual default-REAL values
against which residuals are evaluated. The returned BEM exchange, rather than
an unconverged source guess, is used in the final outdoor fluxes.

Inadmissible trial states are rejected and the source step is shortened.
Nonconvergence after 80 trials invokes the existing WRF/MPAS fatal-error path.
The error identifies latitude, urban category, the last trial's validity and
source, and the last valid source/residual pair. A failed trial is not accepted
through clipping, relaxed tolerances, or an iteration-limit fallback.

The existing SBEMOPTION=1 requirement for TS_SCHEME=1 remains. For SBEMOPTION=0,
the wrapper executes the original core exactly once with no BEM iteration.
No Registry, driver state, or restart-file variable is added.

## HVAC onset and dry indoor states

For a step that reaches a cooling, heating, or dehumidification threshold,
the frozen-load Euler trajectory determines when that threshold is reached.
Only the remaining fraction of the caller's time step contributes to the
step-mean HVAC load. Temperature onset uses the full prognostic indoor heat
capacity. The previous formulas for already-active control, including the
air-only sensible restoration term and the separate humidity relaxation time,
are retained. The same processed loads drive indoor updates, electricity,
and HVAC heat rejection.

Zero and positive indoor specific humidities below `1e-4` are valid states.
The unconditional post-update humidity floor is removed, and the urban entry
preserves zero. The existing cap on the amount HVAC can remove is retained.
A negative or nonfinite prognostic update is rejected, not artificially
humidified. Accepted urban states also respect the next call's admissible
humidity and indoor-temperature ranges; they cannot be accepted only to be
reset or rejected on the following call.

`slucm_bem_calculate` has an optional `state_valid` output. Production coupling
always supplies it and uses the model's parallel fatal-error handling when
necessary. Invalid standalone calls that omit it stop explicitly. The BEM
module still has no WRF- or MPAS-specific module dependency.

## Numerical and coupled WRF verification

The [Intel and coupled WRF validation record](../test/slucm_bem/validation_wrf_ifx_20261113.json)
identifies the tested source and executable hashes, report hashes, comparison
counts, configuration coverage and retained limitations. The two physics
modules and native test programs were unchanged while the documentation was
updated after these tests. All model tests below use `TREEOPTION=0`.

### Native physics tests

The native runner is `test/slucm_bem/run_native.py`. It compiles the actual
urban, BEM, and model-constant modules and the actual URBPARM tables. Only
logging/fatal-error infrastructure and an unused namelist lookup are stubbed.
It compares against the pinned PR base, checks independent heat/water and
HVAC identities, repeats an accepted trial from its original state, and checks
state serialization and intervening calls at another grid cell.

The numerical checks cover ordinary three-class urban, LCZ, distributed
geometry, wet impervious surfaces, green-roof state restoration, winter HVAC
off, humidity zero, threshold onset, and WRF time steps of 6, 30, 60, and
180 seconds. Baseline comparisons include SBEMOPTION=0 and BEM-active
zero-exchange cases. Generated JSON records source/table hashes, compiler,
flags, all commands and the distinction between native results and full WRF
integration.

Run from the updated WRF source directory after loading the usual compiler
environment:

```bash
python3 -I test/slucm_bem/run_native.py --fc ifx \
  --fflags='-O2 -traceback -check bounds -fp-model=strict'
```

GNU Fortran is also supported with `--fc gfortran`. The default baseline is
the pinned PR commit above, which must be available in the checkout's Git
history. Output and logs are retained in a separate generated directory; the
runner does not change model inputs or an existing simulation.

The [2026-11-13 GNU Fortran validation record](../test/slucm_bem/validation_gfortran_20261113.json)
contains source hashes and case inventories. GNU Fortran 14.2 passed both
`-O0 -fcheck=all` and `-O2 -fcheck=bounds` configurations. Each checked 42 BEM
cases, 84 complete urban/BEM cases, 16 expected failure exits, and 56 exact
reference records of 147 real values each. The 76 recorded source solves
converged in 2--9 trials; maximum absolute sensible and latent residuals were
0.005742 and 0.005337 W m-2, respectively.

Intel ifx 2024.0.2 passed the same 42 BEM cases, 84 urban/BEM cases, 16 expected
failure exits and all 56 exact baseline records with the explicit strict
floating-point flags shown above. The initial Intel native run without an
explicit floating-point model failed its exact SBEM0/three-category comparison;
that failure remains recorded. The strict result does not establish exactness
for every compiler option, and the runner's default flags have not been changed.
Native state serialization remains a single-column replay, not a WRF restart
I/O or halo test. Coupled results are reported separately below.

### WRF build and seven coupled runs

The corrected executable was built with ifx/icx 2024.0.2, retaining the existing
`-O3 -fp-model precise` physics configuration. The required changed modules and
their callers were recompiled and linked. This was an incremental build, not
an independent clean rebuild of every retained object; `real.exe` was not
rebuilt. Recompilation of callers is required because optional module
interfaces changed.

Seven model runs completed with 32 Intel MPI ranks in a 4x8 layout and one
OpenMP thread. No fatal or CFL message was reported. All comparisons use the
same executable within each continuous/restart pair.

| Check | Configuration and result |
|---|---|
| Disabled-BEM regression | Summer Noah-MP with distributed morphology, three nested domains, 100/20/4-second steps and 30 minutes of integration. Baseline and candidate matched all 6,678 saved variable records in 21 history-file pairs, with exact schemas. |
| Enabled-BEM outdoor response | The corresponding old/new enabled runs changed TC, QC, urban SH/LH and grid HFX/LH. Maximum absolute changes were 0.302124 K, 0.000906674 kg kg-1, 7.27805/32.2393 W m-2 and 6.74789/28.8833 W m-2, respectively. These are responses to all three corrections together. |
| Candidate restart | Restart at minute 15, continue to minute 30. All 2,862 variable records in nine saved history-file pairs at minutes 20, 25 and 30 were exact. Retained startup/checkpoint differences are described below. |
| Baseline restart supplement | The old executable repeated the same 15-minute split. Its 2,862 post-split variable records were exact, and it reproduced the candidate's reviewed startup resets, CROPCAT discrepancy and attribute differences. |
| Noah/LCZ supplement | Winter Noah standard canyon, 50/10/2-second steps, 30 minutes. It exercised nine observed urban types (1--6 and 8--10), with 27/690/10,093 active urban cells in d01/d02/d03. All specified numerical screens passed. |

The first five runs were four 30-minute continuous runs (old/new, BEM off/on)
and the candidate restart. The two supplemental runs were the baseline
restart and the winter Noah/LCZ integration. The original five-run report keeps
its `CORE_PASS_REVIEW_REQUIRED` status; the separate supplemental audit resolves
the attribution of its residuals without rewriting that report.

The summer enabled run had nonzero QVENT and EVENT in 53,022 nested urban
cell-time samples, but its saved HVAC diagnostics were zero. The winter run
had nonzero QVENT and EVENT in all 64,860 such samples and active heating in
13,338 samples, first appearing in the saved minute-20 output. Negative QFB
with positive electricity use follows the implemented heat-pump heating
definition. Latent HVAC and the zero-humidity endpoint were not exercised by
these coupled outputs. Exact within-step threshold timing and very dry-state
preservation are supported by the native tests, not inferred from five-minute
history output.

Raw zero `FRC_URB2D` in the winter inputs was valid: the urban initialization
uses recognized LCZ land classes and the active table. Output checks confirmed
positive fractions and the expected active counts/classes. Types 7 and 11 did
not occur. The checks do not constitute a separate cellwise comparison of
every initialized fraction with its table value.

### Restart residuals retained

At the restart instant, seven fields differ from the continuous run:
`EIN_URB2D`, `EVENT_URB2D`, `LH_URB`, `QC_URB`, `SH_URB`, `U10E` and `V10E`.
The old and corrected executables produce identical restart-side reset
payloads and affected masks for these fields. Their continuous-run values may
differ because the physics changed. In the final checkpoints, 2,746 of 2,749
variable records match; only `CROPCAT` differs, with the same payloads and
spatial difference pattern in the baseline and candidate comparisons.
`START_DATE` and the two recorded alarm-interval attribute differences are
also reproduced in the baseline.

These results establish no additional discrepancy from the three corrections
in this split test. They do not remove the existing resets or establish full
restart-file identity: `full_restart_payloads_equal=false`. Exact history
agreement starts at the first saved output five minutes after restart; every
intervening timestep was not compared.

The supplemental audit reread the candidate's startup residual fields and
final CROPCAT, and used its hash-pinned original reports for the remaining
candidate comparisons. Saved NetCDF outputs remain with the model operator;
the reviewed archives contain reports, logs, configuration and helper code.
There is no retrospective proof that every output byte has remained unchanged
since the original run, and no independent full coupled energy/water budget
was reconstructed from the retained statistics.

## Physical interpretation and remaining scope

TREEOPTION=1 remains outside the validated scope. Its existing clipping and
known tree-model limitations are not resolved by inserting these exchange
sources. The pathological distributed geometry with zero street fraction
and no moisture conductance is rejected explicitly for BEM before division;
the code does not silently change the geometry or add water.

`physical_acceptance` remains false. The winter standard-canyon run reaches
a minimum TC of 254.80571 K (-18.34 degrees Celsius); this is an explicit
physical-review concern. The candidate-only winter case cannot attribute that
minimum to one correction, and the retained summaries do not contain the
co-located forcing, geometry and budget terms needed to assess it. Finite
temperatures and successful integration are not evidence that this temperature
is physically acceptable.

The tests do not resolve the high-TC, U10, forcing-height and input-data issues
identified in ILS. Earlier 48-hour simulations and figures at `6099d9f6`
predate these corrections and remain historical results; the corrected
enabled-BEM response has not been tested for 48 hours or against observations.
