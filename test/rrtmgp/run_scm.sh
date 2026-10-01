#!/usr/bin/env bash
# Requires WRF/main/{wrf,ideal}.exe built with compile em_scm_xy.
set -euo pipefail
wrf_root=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
case_dir=${1:?usage: run_scm.sh NEW_CASE_DIRECTORY [37|4]}
option=${2:-37}
case "$option" in 37|4) ;; *) echo 'radiation option must be 37 or 4' >&2; exit 2;; esac
if [[ -e "$case_dir/namelist.input" ]]; then
  echo "case already contains namelist.input: $case_dir" >&2
  exit 2
fi
for exe in ideal wrf; do
  [[ -x "$wrf_root/main/$exe.exe" ]] || { echo "missing $exe.exe; build em_scm_xy first" >&2; exit 2; }
done
mkdir -p "$case_dir"
case_dir=$(cd "$case_dir" && pwd)
cp "$wrf_root/test/rrtmgp/namelist.scm37" "$case_dir/namelist.input"
if [[ "$option" == 4 ]]; then
  sed -i -E 's/(ra_(lw|sw)_physics[[:space:]]*=[[:space:]]*)37/\14/' "$case_dir/namelist.input"
fi
cp "$wrf_root/test/rrtmgp/radiation_iofields.txt" "$case_dir/radiation_iofields.txt"
for name in input_sounding input_soil force_ideal.nc; do
  cp "$wrf_root/test/em_scm_xy/$name" "$case_dir/$name"
done
for source in "$wrf_root"/run/*; do
  [[ -f "$source" ]] || continue
  name=${source##*/}
  [[ -e "$case_dir/$name" || -L "$case_dir/$name" ]] && continue
  ln -s "$source" "$case_dir/$name"
done
cd "$case_dir"
"$wrf_root/main/ideal.exe" > ideal.log 2>&1
"$wrf_root/main/wrf.exe" > wrf.log 2>&1
rg 'SUCCESS COMPLETE WRF' wrf.log
printf 'SCM case: %s\n' "$case_dir"
