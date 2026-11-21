#!/usr/bin/env python3
"""Compile full WRF urban physics and verify green-roof/BEM transfer.

This is a native single-column regression, not a full WRF/Noah/MPI or
WRF restart-file acceptance test. Python standard library only.
"""
from __future__ import print_function
import argparse
import hashlib
import json
import os
from pathlib import Path
import shlex
import shutil
import subprocess
import sys
import tempfile
import time

HERE=Path(__file__).resolve().parent
BASELINE='9a6f9edc660e9574dcd4a51a4ba91c7025f24ea6'
PINNED={
 'phys/module_sf_urban.F':'b6de15c191255c2ae9b8a6ff953791479a313df5a3a19ce987a7ef6d22a6b2d1',
 'phys/module_sf_slucm_bem.F':'d2d8d5d53367d28947353f42de3e0e69ca1e650364687ea58be546f8f86a68ea',
}
SCOPE={'actual_complete_urban_module':True,'actual_bem_module':True,
 'actual_constants_and_tables':True,'synthetic_forcing':True,'treeoption':0,
 'full_wrf_build':False,'noah_noahmp_driver_integration':False,'mpi':False,
 'wrf_restart_io':False,'single_column_serialization_only':True,'physical_acceptance':False}

def require(c,m):
 if not c:raise RuntimeError(m)
def digest(b):return hashlib.sha256(b).hexdigest()
def execute(cmd,cwd,log,timeout=180):
 start=time.time()
 r=subprocess.run([str(x) for x in cmd],cwd=str(cwd),stdout=subprocess.PIPE,stderr=subprocess.STDOUT,timeout=timeout)
 log.write_bytes(r.stdout)
 return r,{'command':[str(x) for x in cmd],'cwd':str(cwd),'log':str(log),'returncode':r.returncode,'seconds':time.time()-start}

def main():
 p=argparse.ArgumentParser(description=__doc__)
 p.add_argument('--repo',type=Path,default=HERE.parents[1])
 p.add_argument('--baseline-source-dir','--baseline-dir',dest='baseline_dir',type=Path,help='Extracted original source; required exact pinned hashes are checked')
 p.add_argument('--fc',default=os.environ.get('FC'))
 p.add_argument('--fflags',help='Replace default compiler options (-O0 with checks/traps for GNU, -O2 strict for Intel); GNU test builds always append -ffp-contract=off')
 p.add_argument('--output-dir',type=Path)
 a=p.parse_args();repo=a.repo.resolve()
 out=a.output_dir.resolve() if a.output_dir else Path(tempfile.mkdtemp(prefix='wrf-greenroof-'))
 if a.output_dir:
  require(not out.exists(),'Choose a new output directory: '+str(out));out.mkdir(parents=True)
 report={'kind':'wrf_greenroof_native_v1','passed':False,'scope':SCOPE,'baseline_commit':BASELINE,
         'sources':{},'compilations':[],'runs':[],'exact_snapshots':[]}
 reportpath=out/'greenroof_report.json'
 try:
  compiler=a.fc or next((shutil.which(x) for x in ('ifx','ifort','gfortran') if shutil.which(x)),None)
  require(compiler is not None,'No supported Fortran compiler found')
  compiler=shutil.which(compiler) or str(Path(compiler).resolve())
  v,_=execute([compiler,'--version'],out,out/'compiler.log')
  version=v.stdout.decode(errors='replace');report['compiler']={'path':compiler,'version':version.strip()}
  if 'GNU Fortran' in version:
   required=['-cpp','-ffree-form','-ffree-line-length-none'];default=['-O0','-g','-fcheck=all','-fbacktrace','-ffpe-trap=invalid,zero']
  elif 'Intel' in version or 'ifx' in version.lower() or 'ifort' in version.lower():
   required=['-fpp','-free','-extend-source'];default=['-O2','-traceback','-check','bounds','-fp-model=strict']
  else:raise RuntimeError('Unsupported compiler family')
  flags=required+(shlex.split(a.fflags) if a.fflags else default)
  # Test-only arithmetic: compare baseline and candidate without target-dependent
  # FMA contraction; production WRF build options are not changed.
  if 'GNU Fortran' in version:flags.append('-ffp-contract=off')
  report['compiler']['flags']=flags
  baseline={}
  for rel,expected in PINNED.items():
   if a.baseline_dir:raw=(a.baseline_dir/rel).read_bytes()
   else:
    r=subprocess.run(['git','show',BASELINE+':'+rel],cwd=str(repo),stdout=subprocess.PIPE,stderr=subprocess.PIPE)
    require(r.returncode==0,'Pinned reference missing from local Git history: '+BASELINE)
    raw=r.stdout
   require(digest(raw)==expected,'Pinned baseline mismatch: '+rel)
   baseline[rel]=raw
  sources={}
  for rel in ('phys/module_sf_urban.F','phys/module_sf_slucm_bem.F','phys/module_sf_greenroof_bem.F',
              'share/module_model_constants.F','run/URBPARM.TBL','run/URBPARM_LCZ.TBL'):
   raw=(repo/rel).read_bytes();sources[rel]=raw;report['sources'][rel]={'sha256':digest(raw),'bytes':len(raw),'path':str(repo/rel)}
  for name in ('urban_greenroof_regression.F90','wrf_test_stubs.F90','greenroof_column_invariants.F90','greenroof_column_status.F90'):
   source=HERE/name
   if not source.exists():source=repo/'test/slucm_bem'/name
   raw=source.read_bytes();sources[name]=raw;report['sources'][name]={'sha256':digest(raw),'bytes':len(raw),'path':str(source)}
  runner=Path(__file__).resolve();raw=runner.read_bytes()
  report['sources']['run_greenroof.py']={'sha256':digest(raw),'bytes':len(raw),'path':str(runner)}
  exes={}
  for variant in ('baseline','candidate'):
   build=out/variant;build.mkdir()
   order=[('wrf_test_stubs.F90',sources['wrf_test_stubs.F90']),('module_model_constants.F',sources['share/module_model_constants.F'])]
   if variant=='candidate':order.append(('module_sf_greenroof_bem.F',sources['phys/module_sf_greenroof_bem.F']))
   order += [(Path(rel).name,sources[rel] if variant=='candidate' else baseline[rel]) for rel in ('phys/module_sf_slucm_bem.F','phys/module_sf_urban.F')]
   order.append(('urban_greenroof_regression.F90',sources['urban_greenroof_regression.F90']))
   paths=[]
   for name,raw in order:
    path=build/name;path.write_bytes(raw);paths.append(path)
   exe=build/'urban_greenroof.exe';defs=['-DBASELINE_ONLY'] if variant=='baseline' else []
   r,record=execute([compiler]+flags+defs+paths+['-o',exe],build,build/'compile.log');report['compilations'].append(record)
   require(r.returncode==0,'Fortran compilation failed: '+record['log']);exes[variant]=exe
   for name in ('URBPARM.TBL','URBPARM_LCZ.TBL'):(build/name).write_bytes(sources['run/'+name])
  column_flags=[x.replace('-ffpe-trap=invalid,zero','-ffpe-trap=invalid,zero,overflow') for x in flags]
  for fixture,marker in [('greenroof_column_invariants.F90','GREENROOF_COLUMN_NATIVE_PASS'),
                         ('greenroof_column_status.F90','GREENROOF_COLUMN_STATUS_PASS rejected=6 insulated=1')]:
   build=out/Path(fixture).stem;build.mkdir()
   mod=build/'module_sf_greenroof_bem.F';mod.write_bytes(sources['phys/module_sf_greenroof_bem.F'])
   src=build/fixture;src.write_bytes(sources[fixture]);exe=build/'column.exe'
   r,record=execute([compiler]+column_flags+[mod,src,'-o',exe],build,build/'compile.log')
   report['compilations'].append(record);require(r.returncode==0,'Column compilation failed: '+record['log'])
   r,record=execute([exe],build,build/'run.log');report['runs'].append(record)
   output=r.stdout.decode(errors='replace')
   require(r.returncode==0 and marker in output,'Column invariants/status failed: '+record['log'])
   if fixture=='greenroof_column_invariants.F90':
    required_markers=['ISOTHERMAL_PASS','STEADY_RESISTANCE_PASS','TRANSIENT_CONSERVATION_PASS',
                      'SURFACE_DERIVATIVE_PASS','TIME_REFINEMENT_PASS']
    require(all(output.splitlines().count(x)==1 for x in required_markers),'Incomplete column invariant inventory')
    record['cases']=required_markers
    record['metrics']={line.split('=',1)[0]:float(line.split('=',1)[1]) for line in output.splitlines() if '=' in line}
   else:record['expected_invalid_status_count']=6;record['insulated_column_valid']=True
  for table,ncat in [('3urban',3),('lcz',10)]:
   for mode in ('snapshot_disabled','snapshot_nonbem_green','snapshot_fgr0'):
    snapshots={}
    for variant in ('baseline','candidate'):
     exe=exes[variant]
     r,record=execute([exe,mode,table],exe.parent,exe.parent/(mode+'_'+table+'.log'));report['runs'].append(record)
     output=r.stdout.decode(errors='replace')
     require(r.returncode==0 and 'WRF_GREENROOF_SNAPSHOT_PASS' in output,'Snapshot run failed: '+record['log'])
     raw=(exe.parent/'regression_state.bin').read_bytes();snapshots[variant]=raw
     require(len(raw)==ncat*2*12*147*4,'Snapshot length mismatch')
    require(snapshots['baseline']==snapshots['candidate'],'Disabled/nonBEM path changed: '+mode+' '+table)
    report['exact_snapshots'].append({'mode':mode,'table':table,'records':ncat*2*12,'values_per_record':147,'sha256':digest(snapshots['candidate']),'passed':True})
   exe=exes['candidate'];r,record=execute([exe,'suite',table],exe.parent,exe.parent/('suite_'+table+'.log'))
   report['runs'].append(record);output=r.stdout.decode(errors='replace')
   expected=ncat*4*3+3
   require(r.returncode==0 and 'WRF_GREENROOF_NATIVE_PASS cases='+str(expected) in output,'Greenroof suite failed: '+record['log'])
   cases=[line.partition('WRF_URBAN_CASE_PASS ')[2] for line in output.splitlines() if line.startswith('WRF_URBAN_CASE_PASS ')]
   require(len(cases)==expected and len(set(cases))==expected,'Case inventory mismatch');record['cases']=cases
   metrics=[line for line in output.splitlines() if line.startswith('GREENROOF_METRICS ')]
   require(len(metrics)==1,'Missing solver metrics')
   numbers=list(map(float,metrics[0].split()[1:]));record['maxima']=dict(zip(['solver_residual_wm2','surface_residual_wm2','storage_residual_wm2','cell_residual_wm2','indoor_load_mismatch_w_per_m','storage_roundoff_bound_wm2','storage_residual_to_bound_ratio'],numbers))
   for mode in ('invalid_fgr','invalid_dz','invalid_nonfinite_fgr','invalid_distributed'):
    r,record=execute([exe,mode,table],exe.parent,exe.parent/(mode+'_'+table+'.log'));report['runs'].append(record)
    text=r.stdout.decode(errors='replace')
    require(r.returncode!=0 and 'WRF_FATAL:' in text and 'UNEXPECTED_INVALID_ACCEPTANCE' not in text,'Invalid condition not rejected by host fatal handler')
    record['expected_rejection']=True
  for rel,record in report['sources'].items():
   require(digest(Path(record['path']).read_bytes())==record['sha256'],'Source changed during validation: '+rel)
  report['passed']=True
  print('WRF_GREENROOF_NATIVE_PASS')
 except Exception as e:
  report['error']=str(e);print('WRF_GREENROOF_NATIVE_FAIL: '+str(e),file=sys.stderr)
 finally:
  reportpath.write_text(json.dumps(report,indent=2)+'\n');print(str(reportpath))
 return 0 if report['passed'] else 1
if __name__=='__main__':sys.exit(main())
