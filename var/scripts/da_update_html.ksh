#!/bin/ksh
# da_update_html.ksh

# Assuming starting from ./scripts directory
cd ..
svn update
cd build
./clean -a
. ./setup.ksh gnu
echo 3 | ./configure # optimised code
make -r setup
ln -fs inc/* .
# This file causes da_generate_html.pl to stall
rm -rf RELHUM.inc
cd ..
mkdir -p html
scripts/da_generate_html.pl build html
scripts/da_list_refs.ksh html
