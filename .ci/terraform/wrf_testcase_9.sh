#!/bin/bash
su - ubuntu << 'EOF'
mkdir /home/ubuntu/wrf-stuff
cd wrf-stuff/
git clone git@github.com:davegill/wrf-coop.git
cd wrf-coop/
sed -e "s^_GIT_URL_^$GIT_URL^" -e "s/_GIT_BRANCH_/$GIT_BRANCH/" Dockerfile-sed > Dockerfile
csh build.csh /home/ubuntu/wrf-stuff/ /home/ubuntu/wrf-stuff/
date ; ( ./single.csh ; ./test_009s.csh & ./test_009o.csh & ./test_009m.csh & wait ) >output_009 ; date
date ; ./last_only_once.csh >> output_009 ; date
EOF
