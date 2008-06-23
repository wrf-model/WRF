subroutine da_openfile (kunit, cdfile, cdaccess, cdstatus, cdform)

   !------------------------------------------------------------------------
   ! Purpose: open an file and rewind
   !
   !   method:
   !   ------
   !
   !   input:
   !   -----
   !      kunit:          logical unit
   !      cdfile:         name of file for output of discared obs.
   !      cda!ess:       a!ess (sequential or),
   !      cdstatus:       status (old, new or unknown)
   !      cdform:         form (formatted or unformatted)
   !
   !   output:
   !   ------
   !      opened file
   !
   !   common:                      no
   !   -------
   !   external:                    no
   !   --------
   !   references:                  no
   !   ----------
   !
   !   modifications:
   !   --------------
   !       original :  98-07 (f. vandenberghe)
   !       additions : 98-11 norm doctor (f. vandenberghe)
   !--------------------------------------------------------------------------

   implicit none

   integer, intent(in) :: Kunit
   character*(*), intent(in) :: CDFILE, CDACCESS, CDSTATUS, CDFORM

   integer :: iost

   ! 1.  open FILE
   ! -------------

   IOST = 0

   open (unit   = Kunit, &
          FILE   = CDFILE, &
          ACCESS = CDACCESS, &
          STATUS = CDSTATUS, &
          FORM   = CDFORM, &
          ERR    =  2000, &
          iostat = IOST       )
 
   rewind (Kunit)
 
   return
 
  ! 2.  ERROR PROCESSinG
   ! --------------------
 
2000 continue
   call da_error(__FILE__,__LINE__, &
      (/"Cannot open file"//CDFILE/))

end subroutine da_openfile


