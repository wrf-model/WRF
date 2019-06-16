#!/usr/bin/perl -w
# Author : Xin Zhang, MMM/NCAR, 5/12/2010
#

use strict;

my @Subs_list;
my $Sub;
my $line;
#########################################################
# Parse the subroutine table:

while (<DATA>) {
     chomp;
     next if /^#/;
     push @Subs_list, $_; 
}

#printf " We are going to convert following subroutines : \n";
foreach ( @Subs_list ) {
#print $_."\n"; 
}

while ($line = <>) {
  foreach $Sub ( @Subs_list ) {
    #print " replace $Sub\n" if ($line =~ /$Sub/i);
    $line =~ s/\b$Sub/da_$Sub/gi; 
  }
  print $line;
}

exit;

__DATA__
###########################################################################################
# The following subroutines will be add pre-fix da
all_sub_
call_pkg_and_dist
collect_generic_and_call_pkg
collect_fld_and_call_pkg
collect_double_and_call_pkg
collect_real_and_call_pkg
collect_int_and_call_pkg
collect_logical_and_call_pkg
construct_filename
disable_quilting
maybe_remove_colons
med_initialdata_input
med_shutdown_io
med_add_config_info_to_grid
med_hist_out
med_filter_out
med_auxinput_in
med_restart_out
med_latbound_in
module_alloc_space
module_configure
module_scalar_tables
module_nesting
module_machine
module_tiles
module_timing
module_bc
module_io
module_domain
module_comm_dm
module_date_time
module_dm
module_driver_constants
module_comm_nesting_dm
module_state_description
module_wrf_error
module_wrf_quilt
wrf_quilt_find_server
wrf_quilt_server_ready
multi_files
nl_get_
nl_set_
use_package
wrf_write_field
wrf_read_field
wrf_ext_write_field
wrf_ext_read_field
wrf_bdyout
wrf_bdyin
zero_pad
start_domain
Setup_Timekeeping
output_wrf
input_wrf
open_hist_w
open_aux_u
med_setup_step
med_endup_step
debug_io_wrf
wrf_sizeof_integer
wrf_sizeof_real
wrf_sizeof_doubleprecision
wrf_sizeof_logical
get_value_from_pairs
dim_from_memorder
just_patch_
lower_case
has_char
get_bdyzone
wrf_atotime
wrf_dm_patch_domain
wrf_timetoa
wrf_timeinttoa
wrf_clockprint
append_to_filename
init_wrfio
adjust_io_timestr
traverse_statevars_debug
patch_2_outbuf_
outbuf_2_patch_
set_scalar_indices_from_config
tfp_message
get_current_time_string
get_current_grid_name
get_ijk_from_
get_dims_from_grid_id
rsl_comm_iter
rsl_comm_iter_init
reset_dm_debug
set_dm_debug
get_dm_debug
set_wrf_debug_level
get_wrf_debug_level
wrf_debug
use_output_servers
use_input_servers
wrf_quilt_open_for_write_
wrf_quilt_open_for_read
wrf_quilt_inquire_opened
wrf_quilt_inquire_filename
wrf_quilt_io
wrf_quilt_get_next_time
wrf_quilt_get_previous_time
wrf_quilt_set_time
wrf_quilt_get_next_var
wrf_quilt_get_dom_ti_
wrf_quilt_put_dom_ti_
wrf_quilt_get_dom_td_
wrf_quilt_put_dom_td_
wrf_quilt_put_var_ti_
wrf_quilt_get_var_ti_
wrf_quilt_put_var_td_
wrf_quilt_get_var_td_
wrf_quilt_read_field
wrf_quilt_write_field
wrf_quilt_get_var_info
get_mpi_comm_io_groups
get_nio_tasks_in_group
collect_on_comm_debug
wrf_abort
wrf_check_error
wrf_dm_monitor_rank
wrf_dm_on_monitor
wrf_dm_shutdown
wrf_error_fatal
wrf_get_myproc
wrf_get_nproc
wrf_dm_bcast_
wrf_dm_halo
wrf_dm_boundary
wrf_dm_xpose
wrf_dm_define_comms
wrf_dm_decomp1d
wrf_dm_gatherv
wrf_scatterv_
wrf_gatherv_
wrf_get_dm_communicator
wrf_get_dm_iocommunicator
wrf_get_dm_ntasks
wrf_message
wrf_global_to_patch_
wrf_patch_to_global_
wrf_set_dm_communicator
wrf_set_dm_iocommunicator
wrf_termio_dup
inquire_of_wrf_
init_modules
modify_io_masks
write_68
quilting_disabled
wrf_set_dm_quilt_comm
instate_communicators_for_domain
pop_communicators_for_domain
push_communicators_for_domain
store_communicators_for_domain
wrf_get_dm_quilt_comm
wrf_dm_nestexchange_init
set_tiles
wrf_get_dom_ti_integer
is_this_data_ok_to_use
check_which_switch
###########################################################################################
