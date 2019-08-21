#!/bin/bash
date=`date '+%d/%m/%Y_%H:%M:%S'`
echo $date
buildNumber=$1
echo $buildNumber
sudo bash << 'EOF'
echo "${buildNumber}"
EOF

