#!/usr/bin/env python3
"""Compile and test the WRF SLUCM+BEM repair with actual Fortran physics.

Python 3.6+ standard library; ifx, ifort or gfortran; existing Git history at
6099d9f632e2. No MPI, NetCDF, network or real meteorological inputs are used.
The report deliberately distinguishes native numerical tests from full WRF,
Noah/Noah-MP coupling, restart I/O and physical acceptance.
"""
from __future__ import print_function

import argparse
import hashlib
import json
from pathlib import Path
import re
import shlex
import shutil
import subprocess
import sys
import tempfile
import time


HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[1]
BASELINE = "6099d9f632e204cc8ed4b01e1a8adba1da753a47"
BASELINE_HASHES = {
    "phys/module_sf_slucm_bem.F": "a7b13e70e8904616c6e9e36f8e7e80465ff407c99be2c5a4b8ec376d53c6dd03",
    "phys/module_sf_urban.F": "bbcdc5c7082f47602911b2fdf81f756058a4d8ba8eff805eecfd5c786debd78b",
}
SOURCE_PATHS = (
    "phys/module_sf_slucm_bem.F", "phys/module_sf_urban.F",
    "share/module_model_constants.F", "run/URBPARM.TBL", "run/URBPARM_LCZ.TBL",
    "test/slucm_bem/wrf_test_stubs.F90",
    "test/slucm_bem/bem_controls_regression.f90",
    "test/slucm_bem/urban_exchange_regression.F90",
    "test/slucm_bem/run_native.py",
)
BEM_INVALID = (
    "invalid_dt", "invalid_tau", "invalid_geometry", "invalid_humidity",
    "nonfinite_input", "negative_update",
)
URBAN_INVALID = {
    "invalid_negative_qin": "WRF_FATAL:",
    "invalid_nonfinite_qin": "WRF_FATAL:",
    "invalid_allroof_canyon": "nonpositive canyon humidity conductance; check geometry/exchange coefficients",
    "invalid_predicted_qin": "invalid initial coupling trial",
    "invalid_predicted_tin": "invalid initial coupling trial",
}
SCOPE = {
    "actual_bem_module": True,
    "actual_complete_urban_module": True,
    "actual_model_constants_and_tables": True,
    "synthetic_single_column_forcing": True,
    "treeoption": 0,
    "full_wrf_build": False,
    "noah_or_noahmp_driver_integration": False,
    "mpi_execution": False,
    "wrf_restart_io": False,
    "serializing_single_column_call_state_only": True,
    "physical_acceptance": False,
}


class RegressionError(RuntimeError):
    pass


def require(condition, message):
    if not condition:
        raise RegressionError(message)


def sha256(content):
    return hashlib.sha256(content).hexdigest()


def execute(command, cwd, logfile, timeout=180):
    start = time.time()
    result = subprocess.run(command, cwd=str(cwd), stdout=subprocess.PIPE,
                            stderr=subprocess.STDOUT, timeout=timeout)
    logfile.write_bytes(result.stdout)
    return result, {"command": [str(x) for x in command], "cwd": str(cwd),
                    "log": str(logfile), "returncode": result.returncode,
                    "elapsed_seconds": time.time() - start}


def compile_program(work, compiler, flags, source_data, executable, definitions=()):
    work.mkdir(parents=True, exist_ok=True)
    paths = []
    for name, content in source_data:
        path = work / name
        path.write_bytes(content)
        paths.append(str(path))
    target = work / executable
    command = [compiler] + flags + list(definitions) + paths + ["-o", str(target)]
    result, record = execute(command, work, work / (executable + ".compile.log"))
    require(result.returncode == 0, "Fortran compilation failed: " + record["log"])
    return target, record


def run_program(executable, arguments, label, expected_marker=None, invalid_marker=None):
    result, record = execute([str(executable)] + arguments, executable.parent,
                             executable.parent / (label + ".log"), timeout=60)
    output = result.stdout.decode("utf-8", errors="replace")
    if invalid_marker is None:
        require(result.returncode == 0, "Native test failed: " + record["log"])
        if expected_marker:
            require(expected_marker in output, "Missing native completion marker: " + record["log"])
    else:
        require(result.returncode != 0 and invalid_marker in output and
                "UNEXPECTED_INVALID_ACCEPTANCE" not in output,
                "Invalid input was not rejected through the intended path: " + record["log"])
    record["passed"] = True
    return output, record


def case_inventory(output, prefix, expected_count):
    names = [line[len(prefix):].strip() for line in output.splitlines() if line.startswith(prefix)]
    require(len(names) == expected_count and len(set(names)) == expected_count,
            "Native test case inventory is incomplete or duplicated")
    return names


def coupling_rows(output):
    rows = []
    for line in output.splitlines():
        if not line.startswith("COUPLING "):
            continue
        fields = line.split()
        require(len(fields) == 11, "Malformed native coupling trace")
        numbers = [float(x.replace("D", "E")) for x in fields[3:]]
        rows.append({"case": fields[1], "iterations": int(fields[2]),
                     "residual_w_m2": numbers[:2], "source_w_m2": numbers[2:4],
                     "tc_k": numbers[4], "qc_kg_kg": numbers[5],
                     "sh_w_m2": numbers[6], "lh_w_m2": numbers[7]})
    return rows


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--fc", help="Fortran compiler executable (default: ifx, ifort, then gfortran)")
    parser.add_argument("--fflags", help="Additional compiler flags; default is -O2 with bounds checks and traceback")
    parser.add_argument("--baseline-ref", default=BASELINE,
                        help="Local Git ref supplying the fixed 6099d9f source bytes; hashes must still match")
    parser.add_argument("--output-dir", type=Path, help="New directory for binaries, complete logs and native_report.json")
    args = parser.parse_args()
    if args.output_dir:
        work = args.output_dir.resolve()
        require(not work.exists(), "Output directory already exists; choose a new path: " + str(work))
        work.mkdir(parents=True)
    else:
        work = Path(tempfile.mkdtemp(prefix="wrf-slucm-bem-native-"))
    report = {"kind": "wrf_slucm_bem_native_validation_v1", "passed": False,
              "numerical_conservation_pass": False, "scope": dict(SCOPE),
              "baseline_commit": BASELINE, "baseline_lookup_ref": args.baseline_ref,
              "output_directory": str(work), "compilations": [], "runs": [],
              "exact_snapshot_comparisons": [], "sources": {}}
    report_path = work / "native_report.json"
    try:
        for path in SOURCE_PATHS:
            require((ROOT / path).is_file(), "Missing required source: " + path)
            raw = (ROOT / path).read_bytes()
            report["sources"][path] = {"sha256": sha256(raw), "bytes": len(raw)}
        compiler = args.fc
        if compiler is None:
            compiler = next((shutil.which(name) for name in ("ifx", "ifort", "gfortran")
                             if shutil.which(name)), None)
        require(compiler is not None, "No ifx, ifort or gfortran compiler available")
        compiler = shutil.which(compiler) or compiler
        compiler = str(Path(compiler).resolve())
        version_result, version_record = execute([compiler, "--version"], work, work / "compiler_version.log", timeout=30)
        require(version_result.returncode == 0, "Cannot execute the specified Fortran compiler")
        version = version_result.stdout.decode("utf-8", errors="replace")
        if "GNU Fortran" in version:
            family = "gnu"
            required = ["-cpp", "-ffree-form", "-ffree-line-length-none"]
            default = ["-O2", "-g", "-fcheck=bounds", "-fbacktrace"]
        elif "ifx" in version.lower() or "ifort" in version.lower() or "Intel" in version:
            family = "intel"
            required = ["-fpp", "-free", "-extend-source"]
            default = ["-O2", "-traceback", "-check", "bounds"]
        else:
            raise RegressionError("Supported compilers: ifx, ifort, gfortran; received " + version[:100])
        flags = required + (shlex.split(args.fflags) if args.fflags is not None else default)
        report["compiler"] = {"path": compiler, "family": family, "version": version.strip(), "flags": flags}
        require(re.fullmatch(r"[A-Za-z0-9_./-]+", args.baseline_ref) is not None,
                "Invalid baseline Git ref syntax")
        baseline_sources = {}
        for path, expected in BASELINE_HASHES.items():
            result = subprocess.run(["git", "show", args.baseline_ref + ":" + path], cwd=str(ROOT),
                                    stdout=subprocess.PIPE, stderr=subprocess.PIPE)
            require(result.returncode == 0,
                    "Pinned baseline is missing from this checkout: " + BASELINE + ". Obtain its Git history before testing.")
            require(sha256(result.stdout) == expected, "Pinned baseline source differs: " + path)
            baseline_sources[path] = result.stdout
        report["baseline_sources"] = dict(BASELINE_HASHES)
        original_bem = baseline_sources["phys/module_sf_slucm_bem.F"]
        require(original_bem.count(b"MODULE module_sf_slucm_bem") == 2,
                "Unexpected baseline module declaration")
        renamed_bem = original_bem.replace(b"MODULE module_sf_slucm_bem", b"MODULE module_sf_slucm_bem_reference")
        candidate_bem = (ROOT / "phys/module_sf_slucm_bem.F").read_bytes()
        candidate_urban = (ROOT / "phys/module_sf_urban.F").read_bytes()
        bem_sources = [
            ("module_sf_slucm_bem_reference.f90", renamed_bem),
            ("module_sf_slucm_bem.F", candidate_bem),
            ("bem_controls_regression.f90", (HERE / "bem_controls_regression.f90").read_bytes()),
        ]
        bem_exe, record = compile_program(work / "bem", compiler, flags, bem_sources, "bem_controls.exe")
        report["compilations"].append(record)
        output, record = run_program(bem_exe, [], "controls", "WRF_BEM_CONTROLS_TEST_PASS cases=42")
        record["cases"] = case_inventory(output, "WRF_BEM_CASE_PASS ", 42)
        report["runs"].append(record)
        for mode in BEM_INVALID:
            _, record = run_program(bem_exe, [mode], mode, invalid_marker="slucm_bem_calculate:")
            report["runs"].append(record)

        infrastructure = [
            ("wrf_test_stubs.F90", (HERE / "wrf_test_stubs.F90").read_bytes()),
            ("module_model_constants.F", (ROOT / "share/module_model_constants.F").read_bytes()),
        ]
        driver = (HERE / "urban_exchange_regression.F90").read_bytes()
        executables = {}
        for name in ("candidate", "baseline"):
            directory = work / name
            sources = infrastructure + [
                ("module_sf_slucm_bem.F", candidate_bem if name == "candidate" else original_bem),
                ("module_sf_urban.F", candidate_urban if name == "candidate" else baseline_sources["phys/module_sf_urban.F"]),
                ("urban_exchange_regression.F90", driver),
            ]
            definitions = () if name == "candidate" else ("-DBASELINE_ONLY",)
            executable, record = compile_program(directory, compiler, flags, sources, "urban_exchange.exe", definitions)
            report["compilations"].append(record)
            for table in ("URBPARM.TBL", "URBPARM_LCZ.TBL"):
                (directory / table).write_bytes((ROOT / "run" / table).read_bytes())
            executables[name] = executable
        for table, expected in (("3urban", 28), ("lcz", 56)):
            output, record = run_program(executables["candidate"], ["suite", table], "suite_" + table,
                                         "WRF_URBAN_EXCHANGE_TEST_PASS cases=" + str(expected))
            record["cases"] = case_inventory(output, "WRF_URBAN_CASE_PASS ", expected)
            record["coupling"] = coupling_rows(output)
            report["runs"].append(record)
            for mode, failure_marker in sorted(URBAN_INVALID.items()):
                _, record = run_program(executables["candidate"], [mode, table], mode + "_" + table,
                                         invalid_marker=failure_marker)
                report["runs"].append(record)
        for mode in ("sbem0", "zeroexchange"):
            for table, expected_records in (("3urban", 7), ("lcz", 21)):
                snapshots = {}
                for name in ("baseline", "candidate"):
                    output, record = run_program(executables[name], [mode, table], mode + "_" + table,
                        "WRF_URBAN_SNAPSHOT_PASS records=" + str(expected_records) + " values=147")
                    snapshot_path = executables[name].parent / "regression_state.bin"
                    raw = snapshot_path.read_bytes()
                    require(len(raw) == expected_records * 147 * 4,
                            "Snapshot precision/coverage differs; default REAL must remain 32-bit")
                    retained = executables[name].parent / (mode + "_" + table + ".bin")
                    snapshot_path.rename(retained)
                    snapshots[name] = raw
                    record["snapshot_path"] = str(retained)
                    report["runs"].append(record)
                require(snapshots["baseline"] == snapshots["candidate"],
                        "Exact baseline regression failed: " + mode + " " + table)
                report["exact_snapshot_comparisons"].append({
                    "mode": mode, "table": table, "records": expected_records,
                    "values_per_record": 147, "exact": True,
                    "sha256": sha256(snapshots["candidate"]),
                })
        # A run must describe the files it actually compiled, not later edits.
        for path, before in report["sources"].items():
            require(sha256((ROOT / path).read_bytes()) == before["sha256"],
                    "Source changed during native validation: " + path)
        report["passed"] = True
        report["numerical_conservation_pass"] = True
        report["counts"] = {"bem_cases": 42, "urban_cases": 84, "expected_failure_exits": 16,
                            "exact_baseline_records": 56, "values_per_record": 147}
        print("WRF_SLUCMBEM_NATIVE_PASS")
        print("42 BEM + 84 urban cases; 16 expected failure exits; 56 exact baseline records x 147 values")
        print("Scope: native single-column physics; full WRF/MPI/restart/physical acceptance remain pending")
        return_code = 0
    except (RegressionError, OSError, subprocess.SubprocessError) as error:
        report["error"] = str(error)
        print("WRF_SLUCMBEM_NATIVE_FAIL: " + str(error), file=sys.stderr)
        return_code = 1
    report_path.write_text(json.dumps(report, ensure_ascii=False, indent=2) + "\n")
    print("Report: " + str(report_path))
    return return_code


if __name__ == "__main__":
    try:
        sys.exit(main())
    except RegressionError as error:
        print("WRF_SLUCMBEM_NATIVE_FAIL: " + str(error), file=sys.stderr)
        sys.exit(1)
