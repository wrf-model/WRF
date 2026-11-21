! Parallel hosts must receive an invalid status, not a serial ERROR STOP.
program greenroof_column_status
 use module_sf_greenroof_bem,only:greenroof_column_step,greenroof_bem_diagnostics
 use,intrinsic::ieee_arithmetic,only:ieee_value,ieee_quiet_nan
 implicit none
 real(8)::dz(4),cap(4),cond(4),old(4),new(4),slope,dt,ts,hc,hr
 integer::i
 logical::valid
 type(greenroof_bem_diagnostics)::d
 do i=1,6
  dz=[.05_8,.1_8,.15_8,.2_8];cap=2.e6_8;cond=.5_8;old=300._8
  dt=60._8;ts=305._8;hc=3._8;hr=5._8
  select case(i)
  case(1);dt=0._8
  case(2);ts=ieee_value(0._8,ieee_quiet_nan)
  case(3);dz(2)=ieee_value(0._8,ieee_quiet_nan)
  case(4);cond(3)=-1._8
  case(5);hc=-1._8
  case(6);cap(4)=0._8
  end select
  valid=.true.
  call greenroof_column_step(dt,dz,cap,cond,old,ts,295._8,hc,hr,new,d,slope,valid)
  if(valid.or.d%active.or.slope/=0._8) error stop 'invalid column did not return false status'
  if(any(new/=old)) error stop 'invalid column mutated old temperature'
 enddo
 dz=[.05_8,.1_8,.15_8,.2_8];cap=2.e6_8;cond=.5_8;old=300._8
 call greenroof_column_step(60._8,dz,cap,cond,old,305._8,295._8,0._8,0._8,new,d,slope,valid)
 if(.not.valid.or..not.d%active) error stop 'insulated valid column was rejected'
 if(d%roof_to_indoor_wm2/=0._8) error stop 'insulated bottom leaked heat'
 if(abs(d%solver_residual_wm2)>1.e-7_8) error stop 'insulated column balance'
 print '(A)','GREENROOF_COLUMN_STATUS_PASS rejected=6 insulated=1'
end program greenroof_column_status
