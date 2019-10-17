#! /bin/bash
su - ubuntu << 'EOF'
export GIT_URL=https://github.com/davegill/WRF.git
export GIT_BRANCH=release-v4.1.3
echo $GIT_URL
echo $GIT_BRANCH
mkdir /home/ubuntu/wrf-stuff
cd wrf-stuff/
git clone git@github.com:davegill/wrf-coop.git
cd wrf-coop/
sed -e "s^_GIT_URL_^${GIT_URL}^" -e "s/_GIT_BRANCH_/${GIT_BRANCH}/" Dockerfile-sed > Dockerfile
csh build.csh /home/ubuntu/wrf-stuff/ /home/ubuntu/wrf-stuff/
date ; ( ./single.csh ; ./test_001s.csh & ./test_001o.csh & ./test_001m.csh & wait ) >output_001 ; date
bash my_script.sh output_001 80
EOF
