#!/bin/bash
# Process upload and download data transfers collected with NVCOMPILER_ACC_NOTIFY
 set -e

 f=$1       # file name where the data is
 awk '/^.*load/ {f=$5; gsub(/.*=/,"",f); l=$6; gsub(/.*=/,"",l);
                 v=$9; gsub(/.*=/,"",v); b=$10; gsub(/.*=/,"",b);
      printf("%-8s %-13s %s %-34s %s\n", $1, f, l, v, b)}' $f | sort | uniq -c
