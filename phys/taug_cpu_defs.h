#ifndef _ACCEL
      integer :: ncol__,nlayers__,nbndlw__,ngptlw__
! changed to arguments for thread safety (could reduce this list a bit)
# ifndef ncol__
#   define ncol__ CHNK
# endif
      real  :: pavel(ncol__, nlayers__)
      real  :: wx1(ncol__,nlayers__)
      real  :: wx2(ncol__,nlayers__)
      real  :: wx3(ncol__,nlayers__)
      real  :: wx4(ncol__,nlayers__)
      real  :: coldry(ncol__, nlayers__)
      integer  :: laytrop(ncol__)
      integer  :: jp(ncol__,nlayers__)
      integer  :: jt(ncol__,nlayers__)
      integer  :: jt1(ncol__,nlayers__)
      real  :: colh2o(ncol__,nlayers__)
      real  :: colco2(ncol__,nlayers__)
      real  :: colo3(ncol__,nlayers__)
      real  :: coln2o(ncol__,nlayers__)
      real  :: colco(ncol__,nlayers__)
      real  :: colch4(ncol__,nlayers__)
      real  :: colo2(ncol__,nlayers__)
      real  :: colbrd(ncol__,nlayers__)
      integer  :: indself(ncol__,nlayers__)
      integer  :: indfor(ncol__,nlayers__)
      real  :: selffac(ncol__,nlayers__)
      real  :: selffrac(ncol__,nlayers__)
      real  :: forfac(ncol__,nlayers__)
      real  :: forfrac(ncol__,nlayers__)
      integer  :: indminor(ncol__,nlayers__)
      real  :: minorfrac(ncol__,nlayers__)
      real  :: scaleminor(ncol__,nlayers__)
      real  :: scaleminorn2(ncol__,nlayers__)
      real  :: fac00(ncol__,nlayers__), fac01(ncol__,nlayers__), fac10(ncol__,nlayers__), fac11(ncol__,nlayers__)
      real  :: rat_h2oco2(ncol__,nlayers__),rat_h2oco2_1(ncol__,nlayers__), &
               rat_h2oo3(ncol__,nlayers__),rat_h2oo3_1(ncol__,nlayers__), &
               rat_h2on2o(ncol__,nlayers__),rat_h2on2o_1(ncol__,nlayers__), &
               rat_h2och4(ncol__,nlayers__),rat_h2och4_1(ncol__,nlayers__), &
               rat_n2oco2(ncol__,nlayers__),rat_n2oco2_1(ncol__,nlayers__), &
               rat_o3co2(ncol__,nlayers__),rat_o3co2_1(ncol__,nlayers__)
                                                      !    Dimensions: (ncol__,nlayers__)
      real  :: tauaa(ncol__, nlayers__, nbndlw__)
                                                      !    Dimensions: (ncol__,nlayers__,ngptlw__)
     
      integer  :: nspad(nbndlw__)
      integer  :: nspbd(nbndlw__)
      real  :: oneminusd 
# undef ncol__
#endif
