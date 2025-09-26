  subroutine da_radzicevar_virtual(virtual,temp,ratmix)
!-----------------------------------------------------------------------
!   This function returns virtual temperature in K, given temperature
!      in K and mixing ratio in kg/kg.
!-----------------------------------------------------------------------
  implicit none
  real,parameter :: eps=0.622
  real :: virtual
  real :: temp
  real :: ratmix

  virtual=temp*(1+0.61*ratmix)
  return
  end subroutine da_radzicevar_virtual
