#! /bin/bash
su - ubuntu << 'EOF'
mkdir /home/ubuntu/wrf-stuff
cd wrf-stuff/
git clone git@github.com:davegill/wrf-coop.git
cd wrf-coop/
csh build.csh /home/ubuntu/wrf-stuff/ /home/ubuntu/wrf-stuff/
cp Dockerfile-second_part Dockerfile
date ; ( ./single.csh ; ./test_001s.csh & ./test_001o.csh & ./test_001m.csh & wait ) >output_001 ; date
EOF   
                                      		   
su - ubuntu << 'EOF'
bash my_script.sh output_001
EOF

#!/bin/bash
su - ubuntu << 'EOF'
mkdir /home/ubuntu/wrf-stuff
cd wrf-stuff/
git clone git@github.com:davegill/wrf-coop.git
cd wrf-coop/
csh build.csh /home/ubuntu/wrf-stuff/ /home/ubuntu/wrf-stuff/
cp Dockerfile-second_part Dockerfile
date ; ( ./single.csh ; ./test_002s.csh & ./test_002o.csh & ./test_002m.csh & wait ) >output_002 ; date
EOF
su -ubuntu << 'EOF' 
bash my_script.sh output_002 117 
EOF

#!/bin/bash
su - ubuntu << 'EOF'
mkdir /home/ubuntu/wrf-stuff
cd wrf-stuff/
git clone git@github.com:davegill/wrf-coop.git
cd wrf-coop/
csh build.csh /home/ubuntu/wrf-stuff/ /home/ubuntu/wrf-stuff/
cp Dockerfile-second_part Dockerfile
date ; ( ./single.csh ; ./test_001s.csh & ./test_001o.csh & ./test_001m.csh & wait ) >output_001 ; date
EOF
bash my_script.sh output_001 118 
EOF

#!/bin/bash
su - ubuntu << 'EOF'
mkdir /home/ubuntu/wrf-stuff
cd wrf-stuff/
git clone git@github.com:davegill/wrf-coop.git
cd wrf-coop/
csh build.csh /home/ubuntu/wrf-stuff/ /home/ubuntu/wrf-stuff/
cp Dockerfile-second_part Dockerfile
date ; ( ./single.csh ; ./test_001m.csh & wait ) >output_001 ; date
cd /home/ubuntu && bash my_script.sh output_001 118 
EOF

 sed -i "3i export GIT_URL=https://github.com/davegill/WRF.git\\nexport GIT_BRANCH=release-v4.1.3"
 sed -i '''9i sed -e "s^_GIT_URL_^${GIT_URL}^" -e "s/_GIT_BRANCH_/${GIT_BRANCH}/" Dockerfile-sed > Dockerfile'''
 
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
