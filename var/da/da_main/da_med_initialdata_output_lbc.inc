subroutine da_med_initialdata_output_lbc (grid , config_flags, out_filename)

   !-----------------------------------------------------------------------
   ! Purpose: Write out LBC condition at the t=0 for 4DVAR with LBC control
   !          We only use the perturbation of LBC
   ! Xin Zhang: 10/8/2010
   !-----------------------------------------------------------------------

   implicit none

   type(domain), intent(inout)                 :: grid
   type (grid_config_rec_type) , intent(inout) :: config_flags       
   character(*),  intent (in),  optional       :: out_filename

#ifdef VAR4D

   integer                :: fid, bid
   character (len=80)     :: file_name

   integer :: julyr, julday
   real    :: gmt

   if (trace_use) call da_trace_entry("da_med_initialdata_output_lbc")

   if (present(out_filename)) then
      file_name = trim(out_filename)
   else
      file_name = 'wrfvar_bdyout'
   end if

   ! We need to transfer LBC perturbation from model_grid to grid for output

   grid%u_bxs = grid%u_bxs + model_grid%g_u_bxs
   grid%u_bxe = grid%u_bxe + model_grid%g_u_bxe
   grid%u_bys = grid%u_bys + model_grid%g_u_bys
   grid%u_bye = grid%u_bye + model_grid%g_u_bye
   grid%v_bxs = grid%v_bxs + model_grid%g_v_bxs
   grid%v_bxe = grid%v_bxe + model_grid%g_v_bxe
   grid%v_bys = grid%v_bys + model_grid%g_v_bys
   grid%v_bye = grid%v_bye + model_grid%g_v_bye
   grid%t_bxs = grid%t_bxs + model_grid%g_t_bxs
   grid%t_bxe = grid%t_bxe + model_grid%g_t_bxe
   grid%t_bys = grid%t_bys + model_grid%g_t_bys
   grid%t_bye = grid%t_bye + model_grid%g_t_bye
   grid%mu_bxs = grid%mu_bxs + model_grid%g_mu_bxs
   grid%mu_bxe = grid%mu_bxe + model_grid%g_mu_bxe
   grid%mu_bys = grid%mu_bys + model_grid%g_mu_bys
   grid%mu_bye = grid%mu_bye + model_grid%g_mu_bye
   grid%ph_bxs = grid%ph_bxs + model_grid%g_ph_bxs
   grid%ph_bxe = grid%ph_bxe + model_grid%g_ph_bxe
   grid%ph_bys = grid%ph_bys + model_grid%g_ph_bys
   grid%ph_bye = grid%ph_bye + model_grid%g_ph_bye
   grid%moist_bxs = grid%moist_bxs + model_grid%g_moist_bxs
   grid%moist_bxe = grid%moist_bxe + model_grid%g_moist_bxe
   grid%moist_bys = grid%moist_bys + model_grid%g_moist_bys
   grid%moist_bye = grid%moist_bye + model_grid%g_moist_bye

   grid%u_btxs = grid%u_btxs + model_grid%g_u_btxs
   grid%u_btxe = grid%u_btxe + model_grid%g_u_btxe
   grid%u_btys = grid%u_btys + model_grid%g_u_btys
   grid%u_btye = grid%u_btye + model_grid%g_u_btye
   grid%v_btxs = grid%v_btxs + model_grid%g_v_btxs
   grid%v_btxe = grid%v_btxe + model_grid%g_v_btxe
   grid%v_btys = grid%v_btys + model_grid%g_v_btys
   grid%v_btye = grid%v_btye + model_grid%g_v_btye
   grid%t_btxs = grid%t_btxs + model_grid%g_t_btxs
   grid%t_btxe = grid%t_btxe + model_grid%g_t_btxe
   grid%t_btys = grid%t_btys + model_grid%g_t_btys
   grid%t_btye = grid%t_btye + model_grid%g_t_btye
   grid%mu_btxs = grid%mu_btxs + model_grid%g_mu_btxs
   grid%mu_btxe = grid%mu_btxe + model_grid%g_mu_btxe
   grid%mu_btys = grid%mu_btys + model_grid%g_mu_btys
   grid%mu_btye = grid%mu_btye + model_grid%g_mu_btye
   grid%ph_btxs = grid%ph_btxs + model_grid%g_ph_btxs
   grid%ph_btxe = grid%ph_btxe + model_grid%g_ph_btxe
   grid%ph_btys = grid%ph_btys + model_grid%g_ph_btys
   grid%ph_btye = grid%ph_btye + model_grid%g_ph_btye
   grid%moist_btxs = grid%moist_btxs + model_grid%g_moist_btxs
   grid%moist_btxe = grid%moist_btxe + model_grid%g_moist_btxe
   grid%moist_btys = grid%moist_btys + model_grid%g_moist_btys
   grid%moist_btye = grid%moist_btye + model_grid%g_moist_btye

   call open_w_dataset (bid, trim(file_name), grid , config_flags , &
                        output_boundary , "DATASET=BOUNDARY", ierr )

   if (ierr /= 0) then
      write(unit=message(1),fmt=*) 'Error opening ', &
            trim(file_name),' for writing ierr=',ierr
      call da_error(__FILE__,__LINE__,message(1:1))
   endif

   start_date=current_date

   call geth_julgmt(julyr, julday, gmt)
   config_flags%gmt = gmt
   config_flags%julyr = julyr
   config_flags%julday = julday
   call nl_set_bdyfrq ( grid%id , REAL(model_config_rec%interval_seconds) )
   call model_to_grid_config_rec (grid%id , model_config_rec , config_flags)

   call output_boundary (bid, grid , config_flags , ierr)

   call close_dataset (bid , config_flags, "DATASET=BOUNDARY")

   if (trace_use) call da_trace_exit("da_med_initialdata_output_lbc")

#endif
end subroutine da_med_initialdata_output_lbc


