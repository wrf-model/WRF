!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
! This file contains a list of the dummy arguments for
! solve or any routine (there are others) that take
! all of the state data through thier argument list.  A
! subroutine definition should look like this:
!
! SUBROUTINE solve ( grid ,                      &
! !
! #include "solve_dummy_args.inc"
! !
!                  )
!
! Please note that the comment lines around the include are necessary.
! These arguments are defined by including solve_dummy_arg_defines.inc
! in the declaration section of the subroutine.
!
! Contains dummy arguments to solve.
!
  chem_1 , &
  chem_2 , &
  moist_1 , &
  moist_2 , &
, ru_1 , ru_2 , rv_1 , rv_2  & 
, rw_1 , rw_2 , rrp_1 , rrp_2  & 
, rr_1 , rr_2 , rtp_1 , rtp_2  & 
, rom_1 , rom_2 , ru_1 , ru_2  & 
, rv_1 , rv_2 , rw_1 , rw_2  & 
, rrp_1 , rrp_2 , rr_1 , rr_2  & 
, rtp_1 , rtp_2 , rom_1 , rom_2  & 
, pip , pp , du , dv  & 
, pib , r , rtb , rrb  & 
, zx , zy , z , pb  & 
, h_diabatic , zeta_z , z_zeta , msft  & 
, msfu , msfv , f , e  & 
, sina , cosa , ht , cofwr  & 
, cofrz , u_base , v_base , qv_base  & 
, rdzu , rdzw , fzm , fzp  & 
, zeta , zetaw , dzeta , dzetaw  & 
, rdx , rdy , dts , dtseps  & 
, resm , zetatop , cf1 , cf2  & 
, cf3 , sp31 , ep31 , sp32  & 
, ep32 , sp33 , ep33 , sp34  & 
, ep34 , sp21 , ep21 , sp22  & 
, ep22 , sp23 , ep23 , sp11  & 
, ep11 , sp12 , ep12 , sm31  & 
, em31 , sm32 , em32 , sm33  & 
, em33 , sm34 , em34 , sm21  & 
, em21 , sm22 , em22 , sm23  & 
, em23 , sm11 , em11 , sm12  & 
, em12 , number_at_same_level , halo_x , halo_y  & 
, num_tiles , num_tiles_x , num_tiles_y , tiled  & 
, patched , time_step_max , max_dom , dyn_opt  & 
, id , level , s_we , e_we  & 
, s_sn , e_sn , s_vert , e_vert  & 
, time_step_count_start , time_step_count_end , time_step_count_output , time_step_sound  & 
, mother_id , i_mother_start , j_mother_start , i_mother_end  & 
, j_mother_end , mother_grid_ratio , mother_time_step_ratio , moad_grid_ratio  & 
, moad_time_step_ratio , dx , dy , dt  & 
, smdiv , epssm , khdif , kvdif  & 
, periodic_x , symmetric_xs , symmetric_xe , open_xs  & 
, open_xe , periodic_y , symmetric_ys , symmetric_ye  & 
, open_ys , open_ye , nested , specified  & 
, top_radiation , sd31 , ed31 , sd32  & 
, ed32 , sd33 , ed33 , sd34  & 
, ed34 , sd21 , ed21 , sd22  & 
, ed22 , sd23 , ed23 , sd11  & 
, ed11 , sd12 , ed12 , total_time_steps  & 
