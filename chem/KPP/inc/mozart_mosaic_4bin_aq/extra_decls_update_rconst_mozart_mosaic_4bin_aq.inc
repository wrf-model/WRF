!
   real(kind=dp), intent(in) :: rh
   REAL(KIND=dp), INTENT(IN) ::  nume,den

   INTEGER, PARAMETER       :: vbs_nbin = 4, vbs_nspec = 9,              &
                               vbs_alk4 = 1, vbs_alk5 = 2, vbs_ole1 = 3, &
                               vbs_ole2 = 4, vbs_aro1 = 5, vbs_aro2 = 6, &
                               vbs_isop = 7, vbs_sesq = 8, vbs_terp = 9, &
                               vbs_c1   = 1, vbs_c10  = 2, vbs_c100 = 3, &
                               vbs_c1000 = 4
