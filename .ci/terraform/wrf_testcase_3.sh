#!/bin/bash
su - ubuntu << 'EOF'
mkdir /home/ubuntu/wrf-stuff
cd wrf-stuff/
git clone git@github.com:davegill/wrf-coop.git
cd wrf-coop/
sed -e "s^_GIT_URL_^$GIT_URL^" -e "s/_GIT_BRANCH_/$GIT_BRANCH/" Dockerfile-sed > Dockerfile
csh build.csh /home/ubuntu/wrf-stuff/ /home/ubuntu/wrf-stuff/
date ; ( ./single.csh ; ./test_003s.csh & ./test_003o.csh & ./test_003m.csh & wait ) >output_003 ; date
date ; ./last_only_once.csh >> output_003 ; date
EOF
