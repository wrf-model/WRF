subroutine da_cv_to_vv (cv_size, rcv, mzs, nmzs, vv)

   !---------------------------------------------------------------------------
   ! Purpose: Fill (local) vv arrays from 1D (local) cv array.
   !---------------------------------------------------------------------------

   implicit none

   integer,        intent(in)    :: cv_size     ! Length of CV array.
   real,           intent(in)    :: rcv(1:cv_size) ! Control variables v.
   integer,        intent(in)    :: nmzs        ! number of cv for mz
   integer,        intent(in)    :: mzs(nmzs)   ! Background error structure levels.
   type (vp_type), intent(inout) :: vv          ! Grdipt/EOF cv_array.

   integer   :: is,ie       ! Local grid range in y coordinate.
   integer   :: js,je       ! Local grid range in x coordinate.
   integer   :: ix,jy       ! Local grid horizontal dimensions.
   integer   :: mz          ! Max vertical coordinate for v1 through v5 arrays.
   integer   :: ne          ! Ensemble size.
   integer   :: cv_s,cv_e   ! Starting and ending indices into CV array.
   integer   :: ijm         ! Size of interior of v1 through v5 arrays.
   integer   :: ijmn        ! Size of interior of alpha cv arrays.

   if (trace_use) call da_trace_entry("da_cv_to_vv")

   if( use_rf )then
      is = its
      ie = ite
      js = jts
      je = jte
   else
      call da_error(__FILE__,__LINE__,(/"This subroutine should not be called for use_rf = .false."/))
   endif
   ix = ie-is+1
   jy = je-js+1
   cv_e = 0

   !--------------------------------------------------------------------------
   ! [1] Transfer components of Jb control variable:
   !--------------------------------------------------------------------------

   ! Fill v1
   mz = mzs(1)
   if (mz > 0) then
      ijm = ix * jy * mz
      cv_s = cv_e + 1
      cv_e = cv_s + ijm - 1
      vv % v1(is:ie,js:je,1:mz) = RESHAPE(rcv(cv_s:cv_e),(/ix, jy, mz/))
   end if

   ! Fill v2
   mz = mzs(2)
   if (mz > 0) then
      ijm = ix * jy * mz
      cv_s = cv_e + 1
      cv_e = cv_s + ijm - 1
      vv % v2(is:ie,js:je,1:mz) = RESHAPE(rcv(cv_s:cv_e),(/ix, jy, mz/))
   end if

   ! Fill v3
   mz = mzs(3)
   if (mz > 0) then
      ijm = ix * jy * mz
      cv_s = cv_e + 1
      cv_e = cv_s + ijm - 1
      vv % v3(is:ie,js:je,1:mz) = RESHAPE(rcv(cv_s:cv_e),(/ix, jy, mz/))
   end if

   ! Fill v4
   mz = mzs(4)
   if (mz > 0) then
      ijm = ix * jy * mz
      cv_s = cv_e + 1
      cv_e = cv_s + ijm - 1
      vv % v4(is:ie,js:je,1:mz) = RESHAPE(rcv(cv_s:cv_e),(/ix, jy, mz/))
   end if

   ! Fill v5
   mz = mzs(5)
   if (mz == 1) then ! Can only be 0 or 1 (2D ps_u field)
      ijm = ix * jy * mz
      cv_s = cv_e + 1
      cv_e = cv_s + ijm - 1
      vv % v5(is:ie,js:je,1:mz) = RESHAPE(rcv(cv_s:cv_e),(/ix, jy,mz/))
   end if

   if ( nmzs > 10 ) then
      ! Fill v6
      mz = mzs(6)
      if (mz > 0) then
         ijm = ix * jy * mz
         cv_s = cv_e + 1
         cv_e = cv_s + ijm - 1
         vv % v6(is:ie,js:je,1:mz) = RESHAPE(rcv(cv_s:cv_e),(/ix, jy, mz/))
      end if
      ! Fill v7
      mz = mzs(7)
      if (mz > 0) then
         ijm = ix * jy * mz
         cv_s = cv_e + 1
         cv_e = cv_s + ijm - 1
         vv % v7(is:ie,js:je,1:mz) = RESHAPE(rcv(cv_s:cv_e),(/ix, jy, mz/))
      end if
      ! Fill v8
      mz = mzs(8)
      if (mz > 0) then
         ijm = ix * jy * mz
         cv_s = cv_e + 1
         cv_e = cv_s + ijm - 1
         vv % v8(is:ie,js:je,1:mz) = RESHAPE(rcv(cv_s:cv_e),(/ix, jy, mz/))
      end if
      ! Fill v9
      mz = mzs(9)
      if (mz > 0) then
         ijm = ix * jy * mz
         cv_s = cv_e + 1
         cv_e = cv_s + ijm - 1
         vv % v9(is:ie,js:je,1:mz) = RESHAPE(rcv(cv_s:cv_e),(/ix, jy, mz/))
      end if
      ! Fill v10
      mz = mzs(10)
      if (mz > 0) then
         ijm = ix * jy * mz
         cv_s = cv_e + 1
         cv_e = cv_s + ijm - 1
         vv % v10(is:ie,js:je,1:mz) = RESHAPE(rcv(cv_s:cv_e),(/ix, jy, mz/))
      end if
   end if ! cloud cv

   if ( nmzs == 8 .or. nmzs == 13 ) then
      ! Fill v11
      mz = mzs(nmzs-2)
      if (mz > 0) then
         ijm = ix * jy * mz
         cv_s = cv_e + 1
         cv_e = cv_s + ijm - 1
         vv % v11(is:ie,js:je,1:mz) = RESHAPE(rcv(cv_s:cv_e),(/ix, jy, mz/))
      end if
   end if ! w

   !--------------------------------------------------------------------------
   ! [2] Transfer components of Je control variable:
   !--------------------------------------------------------------------------
   mz = mzs(nmzs-1)
   ne = mzs(nmzs)
   if ( ne > 0 ) then
      ix = ite_int - its_int + 1
      jy = jte_int - jts_int + 1
      ijmn = ix * jy * mz * ne
      cv_s = cv_e + 1
      cv_e = cv_s + ijmn - 1
!     vv % alpha(is:ie,js:je,1:mz,1:ne) = RESHAPE(rcv(cv_s:cv_e),(/ix, jy, mz, ne/))
      vv % alpha(its_int:ite_int,jts_int:jte_int,1:mz,1:ne) = RESHAPE(rcv(cv_s:cv_e),(/ix, jy, mz, ne/))
   end if

   if (trace_use) call da_trace_exit("da_cv_to_vv")

end subroutine da_cv_to_vv


