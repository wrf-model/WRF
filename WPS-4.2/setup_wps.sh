#!/bin/bash

key="$1"

loc_path=$(pwd)

case $key in

antarctica ) echo "setting up for antarctica ..."
             rm -rf $loc_path/geogrid/GEOGRID.TBL
             ln -sf $loc_path/geogrid/GEOGRID.TBL.ANTARCTICA $loc_path/geogrid/GEOGRID.TBL 
             rm -rf $loc_path/metgrid/METGRID.TBL
             ln -sf $loc_path/metgrid/METGRID.TBL.ANTARCTICA $loc_path/metgrid/METGRID.TBL
             rm -rf $loc_path/Vtable
             ln -sf $loc_path/ungrib/Variable_Tables/Vtable.ANTARCTICA $loc_path/Vtable
             ;;
alps )       echo "setting up for alps ..."
             rm -rf $loc_path/geogrid/GEOGRID.TBL
             ln -sf $loc_path/geogrid/GEOGRID.TBL.ALPS_COSMO $loc_path/geogrid/GEOGRID.TBL 
             rm -rf $loc_path/metgrid/METGRID.TBL
             ln -sf $loc_path/metgrid/METGRID.TBL.ALPS_COSMO $loc_path/metgrid/METGRID.TBL
             rm -rf $loc_path/Vtable
             ln -sf $loc_path/ungrib/Variable_Tables/Vtable.ALPS_COSMO $loc_path/Vtable
             ;;

*) echo "argument can be antarctica or alps"
   
esac

