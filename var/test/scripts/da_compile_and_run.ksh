#!/bin/ksh

export DAT_DIR=${DAT_DIR:-~/data}

export FULL=${FULL:-false}
export COMPILE=${COMPILE:-true}
export EXECUTE=${EXECUTE:-true}
export CLEAN=${CLEAN:-true}

# Need a cleaner mapping between compiler and configure options
# Assuming option 2 is pgi mpi is a hack

export TYPE=${TYPE:-opt}
export RELEASE=${RELEASE:-trunk}
export REL_DIR=${REL_DIR:-$HOME/code/$RELEASE}
export REGIONS=${REGIONS:-con200}
export PROCS=${PROCS:-1}
export COMPILERS=${COMPILERS:-g95}
export TARGET=${TARGET:-all}

echo "TYPE      $TYPE"
echo "RELEASE   $RELEASE"
echo "COMPILE   $COMPILE"
echo "FULL      $FULL"
echo "EXECUTE   $EXECUTE"
echo "CLEAN     $CLEAN"
echo "COMPILERS $COMPILERS"
echo "REGIONS   $REGIONS"
echo "PROCS     $PROCS"
echo "TARGET    $TARGET"

###########
# Compiling
###########

let COUNT=1

for COMPILER in $COMPILERS; do
   export ID=${MACHINE}_${COMPILER}_${TYPE}
   export WRFVAR_DIR=$REL_DIR/$ID/wrfvar
   if $COMPILE; then
      OPTION=${OPTIONS[$COUNT]}
      echo "Compiling $WRFVAR_DIR $TARGET with option $OPTION"
      cd $WRFVAR_DIR
      . ./setup.ksh $COMPILER >/dev/null
      svn update #>/dev/null 2>&1
      svn status
      if $FULL; then ./clean -a >/dev/null 2>&1; fi
      echo $OPTION | ./configure $TARGET >configure.out 2>&1
      rm -f build/links
      ./compile $TARGET > compile.out 2>&1
      if $CLEAN; then ./clean > /dev/null 2>&1; fi
      echo $(ls -l build/*.exe | wc -l) executables
      let COUNT=$COUNT+1
   fi
   if $EXECUTE; then
      for REGION in $REGIONS; do
         for NUM_PROCS in $PROCS; do
            export NUM_PROCS
            cd $DAT_DIR/$REGION
            . $WRFVAR_DIR/setup.ksh $COMPILER >/dev/null
            echo "Testing $WRFVAR_DIR on $REGION"
            ./test.ksh
         done
      done
   fi
done
